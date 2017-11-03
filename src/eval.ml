open Core
open Lexing

exception EvaluateError
exception BindError

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

(* 
let rec is_num = Ast.(function
    | TermZero _ -> true
    | TermSucc (_, num) -> is_num num
    | _ -> false) *)

(* let rec eval1 ctx t = match t with
    TmApp(fi,TmAbs(_, x, t12) ,v2) when isval ctx v2 ->
    termSubstTop v2 t12
   | TmApp(fi,v1,t2) when isval ctx v1 ->
    let t2' = eval1 ctx t2 in
    TmApp(fi, v1, t2')
   | TmApp(fi,t1,t2) ->
    let t1' = eval1 ctx t1 in
    TmApp(fi, t1', t2)
   | _ -> 
    raise NoRuleApplies *)

let rec eval' env = Ast.(function
    | TermVar (info, name) -> (match Environment.get name env with
        | None -> raise BindError
        | Some x -> x
      )
    | TermApp (info, TermAbs (_, name, term1), term2) ->
      let closure = Environment.create (Some env) in
      Environment.set closure term2 name;
      eval' closure term1
    | TermApp (info, term1, term2) -> raise EvaluateError
    | x -> x
  )

let eval filename env lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast =
    try
      List.map ~f:(eval' env) (parse lexbuf)
    with
    | Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s%!" msg;
      raise @@ Lexer.SyntaxError msg
    | Parser.Error ->
      Printf.fprintf stderr "Syntax error! [%s] @%s\n" (Lexing.lexeme lexbuf) (Ast.show_info (Lexer.info lexbuf));
      raise @@ Parser.Error
  in
  ast
