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
         else raise @@ TypeError (info, "Type mismatch!")
       | _ ->
         raise @@ TypeError (info, "Arrow type expected!"))
    | TermIf (info, condition, term1, term2) -> (match typeof' env condition with
        | Boolean ->
          let ty1 = typeof' env term1 in
          let ty2 = typeof' env term2 in
          if ty1 = ty2
          then ty2
          else raise @@ TypeError (info, "Alternative clause has type mismatch")
        | _ -> raise @@ TypeError (info, "Condition clause not boolean")
      )
    | TermBool (info, value) -> Boolean)

let typeof env ast =
  ignore @@ typeof' env ast;
  ast

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let rec eval' env = Ast.(function
    | TermVar (info, name) -> (match ValueEnv.get name env with
        | None -> raise @@ BindError (info, name)
        | Some x -> x
      )
    | TermApp (info, TermAbs (_, name, ty, term1), term2) ->
      let closure = ValueEnv.create (Some env) in
      ValueEnv.set closure (eval' env term2) name;
      eval' closure term1 
    | TermApp (info, term1, term2) ->
      let term1 = eval' env term1 in
      let term2 = eval' env term2 in
      eval' env (TermApp (info, term1, term2))
    | TermIf (_, c, t1, t2) -> begin match eval' env c with
        | TermBool (_, true) -> t1
        | TermBool (_, false) -> t1
        (* Raise error because condition clause could recieve only boolean *)
        | _ -> raise EvaluateError
      end
    (* No rules to apply *)
    | TermAbs (_, _, _, _) as x -> x
    | TermBool (_, _) as x -> x
  )

let eval filename lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    lexbuf
    |> parse
    |> List.map ~f:(typeof TypeEnv.empty)
    |> List.map ~f:(eval' ValueEnv.empty)
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
  | Typing.TypeError (info, reason) as e ->
    Printf.fprintf stderr "Type error! [%s] @%s\n" reason (Ast.show_info info);
    raise @@ e
  | EvaluateError as e ->
    Printf.fprintf stderr "Evaluate error [%s] @%s\n" (Lexing.lexeme lexbuf) (Ast.show_info (Lexer.info lexbuf));
    raise @@ e
