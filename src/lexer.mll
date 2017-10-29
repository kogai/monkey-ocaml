{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
  {
    pos with pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "true" { TRUE }
  | "false" { FALSE }
  | "is_zero" { ISZERO }
  | "succ" { SUCC }
  | "pred" { PRED }
  | '0' { ZERO }
  | '(' { PARENTHL }
  | ')' { PARENTHR }
  | _ { raise (SyntaxError ("Unexpected character: [" ^ Lexing.lexeme lexbuf ^ "]")) }
  | eof { EOF }
