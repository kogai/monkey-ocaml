open Core
open Lexing

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let eval' x = ()
(* let eval' = function
   | `TermTrue
   | `TermFalse
   | `TermZero
   | `TermSucc of t
   | `TermPred of t
   | `TermIsZero of t
   | `TermIf of t * t * t
   | `TermIllegal *)

let eval filename lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let statements = parse lexbuf in
  List.iter ~f:eval' statements;
  statements
