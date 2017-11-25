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

let rec typeof' env = Ast.(function
    | TermVar (info, name) -> (match TypeEnv.get name env with
        | None -> raise @@ BindError (info, name)
        | Some x -> x
      )
    | TermAbs (info, name, ty1, term) ->
      TypeEnv.set env ty1 name;
      Arrow (ty1, (typeof' env term))
    | TermApp (info, term1, term2) ->
      let ty1 = typeof' env term1 in
      let ty2 = typeof' env term2 in
      (match ty1 with
       | Arrow (ty1', ty2') ->
         if ty1' = ty2
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
