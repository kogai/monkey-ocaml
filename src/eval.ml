open Core
open Lexing

exception EvaluateError
exception BindError of Ast.info * string 

module Environment : sig
  type t = {
    store: (string, Ast.t) Hashtbl.t;
    outer: t option;
  }
  val create: (t option) -> t
  val empty: t
  val get: string -> t-> Ast.t option
  val set: t -> Ast.t -> string -> unit
end = struct
  type t = {
    store: (string, Ast.t) Hashtbl.t;
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

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let rec eval' env = Ast.(function
    | TermVar (info, name) -> (match Environment.get name env with
        | None -> raise @@ BindError (info, name)
        | Some x -> x
      )
    | TermApp (info, TermAbs (_, name, ty, term1), term2) ->
      let closure = Environment.create (Some env) in
      Environment.set closure (eval' env term2) name;
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

let eval filename env lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    lexbuf
    |> parse
    |> List.map ~f:(Typing.typeof env)
    |> List.map ~f:(eval' env)
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
