open Core
open Lexing

exception EvaluateError
exception BindError of Ast.info * string 
exception TypeError of Ast.info * string

module Environment (Impl : sig type t end) : sig
  type t = {
    store: (string, Impl.t) Hashtbl.t;
    outer: t option;
  }
  (* FIXME: Lack of separate environment *)
  val create: (t option) -> t
  val empty: t
  val get: string -> t-> Impl.t option
  val set: t -> Impl.t -> string -> unit
end = struct
  type t = {
    store: (string, Impl.t) Hashtbl.t;
    outer: t option;
  }

  let create outer = {
    store = String.Table.create ();
    outer;
  }
  let empty = create None

  let rec get name = function
    | { store; outer = None} -> Hashtbl.find store name
    | { store; outer = Some o } ->
      match Hashtbl.find store name with
      | None -> get name o
      | v -> v

  let set env data key =
    Hashtbl.set env.store ~key ~data;
end

module TypeEnv = Environment(struct
    type t = Ast.ty
  end)

module ValueEnv = Environment(struct
    type t = Ast.t
  end)

let rec subtype sub ty =
  sub = ty || Ast.(match (sub, ty) with
      | (Record subs, Record tys) ->
        List.fold
          ~f:(fun acc (name, ty) ->
              let sub = List.find ~f:(fun (n, _) -> n = name) subs in
              acc && (match sub with
                  | None -> false
                  | Some (_, s) -> s = ty)
            )
          ~init:true
          tys
      | (Variant subs, Variant tys) -> false
      | (Arrow (sub1, sub2), Arrow (ty1, ty2)) ->
        false
      | (_, _) -> false
    )

let rec typeof' env = Ast.(function
    | TermVar (info, name) -> (match TypeEnv.get name env with
        | None -> raise @@ BindError (info, name)
        | Some x -> x
      )
    | TermAbs (info, name, ty1, term) ->
      let env' = TypeEnv.create (Some env) in
      TypeEnv.set env' ty1 name;
      Arrow (ty1, (typeof' env' term))
    | TermLet (info, name, t1, t2) ->
      let ty1 = typeof' env t1 in
      TypeEnv.set env ty1 name;
      Arrow (ty1, (typeof' env t2))
    | TermDef (info, name, t) ->
      let ty = typeof' env t in
      TypeEnv.set env ty name;
      ty
    | TermApp (info, term1, term2) ->
      let ty1 = typeof' env term1 in
      let ty2 = typeof' env term2 in
      (match ty1 with
       | Arrow (ty1', ty2') ->
         if subtype ty2 ty1'
         then ty2'
         else
           let reason = Printf.sprintf
               "[%s] and [%s] are imcompatible."
               (string_of_type ty1')
               (string_of_type ty2)
           in
           raise @@ TypeError (info, reason)
       | _ ->
         raise @@ TypeError (info, "Arrow type expected"))
    | TermIf (info, condition, term1, term2) -> (match typeof' env condition with
        | Boolean ->
          let ty1 = typeof' env term1 in
          let ty2 = typeof' env term2 in
          if ty1 = ty2
          then ty2
          else
            let reason = Printf.sprintf
                "[%s] and [%s] which defined at alternative clause are imcompatible."
                (string_of_type ty1)
                (string_of_type ty2)
            in
            raise @@ TypeError (info, reason)
        | t -> raise @@ TypeError (
            info,
            Printf.sprintf "[%s] which defined at condition clause isn't boolean" (string_of_type t))
      )
    | TermTuple (info, terms) ->
      let tys = List.map ~f:(typeof' env) terms in
      Tuple tys
    | TermRecord (info, terms) ->
      let tys = List.map ~f:(fun (label, t) -> (label, (typeof' env t))) terms in
      Record tys
    | TermGet (info, term, name) ->
      let ty = typeof' env term in
      let result = (match ty with
          | Record tys -> tys
                          |> List.find ~f:(fun (n, _) -> n = name)
                          |> Option.map ~f:(Tuple2.get2)
          | Tuple tys ->
            let idx = try (int_of_string name) - 1
              with _ -> raise @@ TypeError (info, Printf.sprintf "tupple only allow access by int but by [%s]" name)
            in
            List.nth tys idx
          | ty -> raise @@ TypeError (info, Printf.sprintf "%s is not record." @@ string_of_type ty)
        ) in
      (match result with
       | None ->
         let reason = match term with
           | TermVar (_, n) -> n
           | _ -> string_of_type ty
         in
         raise @@ TypeError (info, Printf.sprintf "key %s is not exist in %s." name reason)
       | Some ty -> ty
      )
    | TermCase (info, cond, []) ->
      raise @@ TypeError (info, Printf.sprintf "arm of pattern should exist at least one")
    | TermCase (info, cond, branches) ->
      let conds = match (typeof' env cond) with
        | Variant ts -> ts
        | _ -> (typeof' env cond)::[]
      in

      let (_, reducted) = List.fold
          ~f:(fun (conds, acc) (matchable_ty, branch_term) ->
              let is_match c = c = matchable_ty in
              let has_match = List.find ~f:is_match conds in
              (match has_match with
               | None ->
                 let reason =
                   Printf.sprintf
                     "%s isn't belongs to %s"
                     (string_of_type matchable_ty)
                     (string_of_type (typeof' env cond))
                 in
                 raise @@ TypeError (info, reason)
               | Some _ ->
                 let refined = List.filter ~f:(fun c -> not @@ is_match c) conds in
                 let env' = TypeEnv.create (Some env) in
                 let name = match cond with
                   | TermVar (_, n) -> n
                   | x ->
                     let reason = Printf.sprintf "case condition didn't support %s yet" (string_of_t x) in
                     raise @@ TypeError ((get_info x), reason)
                 in
                 TypeEnv.set env' matchable_ty name;
                 let reducted = typeof' env' branch_term in
                 match acc with
                 | None -> (refined, Some reducted)
                 | Some pre when pre <> reducted ->
                   let reason = Printf.sprintf
                       "case branch has type %s but expect %s"
                       (string_of_type reducted)
                       (string_of_type pre)
                   in
                   raise @@ TypeError (info, reason)
                 | acc -> (refined, Some reducted)
              )
            )
          ~init:(conds, None)
          branches
      in
      (match reducted with
       | None -> exit 1
       | Some r -> r)
    | TermInfix (info, op, left, right) -> begin
        match op with
        | "==" ->
          let ty_left = typeof' env left in
          let ty_right = typeof' env right in
          if ty_left = ty_right
          then Boolean
          else raise @@ TypeError (
              info,
              sprintf "infix %s %s %s are imcompatible" (string_of_type ty_left) op (string_of_type ty_right))
        | "-"
        | "+" ->
          let ty_left = typeof' env left in
          let ty_right = typeof' env right in
          if ty_left = ty_right
          then ty_left
          else raise @@ TypeError (
              info,
              sprintf "infix %s %s %s are imcompatible" (string_of_type ty_left) op (string_of_type ty_right))
        | _ -> raise @@ TypeError (info, sprintf "infix operator %s is not supported" op)
      end
    | TermBool _ -> Boolean
    | TermNat _ -> Nat
    | TermUnit _ -> Unit
  )

let typeof env ast =
  try
    ignore @@ typeof' env ast;
    None
  with
  | TypeError (info, reason) ->
    Some (info, reason)

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let eval filename lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    lexbuf
    |> parse
    |> List.map ~f:(typeof TypeEnv.empty)
  with
  | Lexer.SyntaxError msg as e ->
    Printf.fprintf stderr "%s%!" msg;
    raise @@ e
  | Parser.Error as e ->
    Printf.fprintf stderr "Parse error [%s] @%s\n" (Lexing.lexeme lexbuf) (Ast.show_info (Lexer.info lexbuf));
    raise @@ e
  | BindError (info, name) as e ->
    Printf.fprintf stderr "Unbound error! [%s] @%s\n" name (Ast.show_info info);
    raise @@ e
  | EvaluateError as e ->
    Printf.fprintf stderr "Evaluate error [%s] @%s\n" (Lexing.lexeme lexbuf) (Ast.show_info (Lexer.info lexbuf));
    raise @@ e
