{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let { lex_curr_p; lex_curr_pos; } = lexbuf in
  lexbuf.lex_curr_p <- {
    lex_curr_p with pos_bol = lex_curr_pos;
                    pos_lnum = lex_curr_p.pos_lnum + 1
  }

  let info { lex_curr_p; lex_start_pos; } =
    let { pos_fname; pos_lnum; pos_cnum; pos_bol; } = lex_curr_p in
    Ast.createInfo pos_fname pos_lnum (pos_cnum - pos_bol)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "if" { IF (info lexbuf) }
  | "then" { THEN (info lexbuf) }
  | "else" { ELSE (info lexbuf) }
  | "true" { TRUE (info lexbuf) }
  | "false" { FALSE (info lexbuf) }
  | "is_zero" { ISZERO (info lexbuf) }
  | "succ" { SUCC (info lexbuf) }
  | "pred" { PRED (info lexbuf) }
  | '0' { ZERO (info lexbuf) }
  | '(' { PARENTHL (info lexbuf) }
  | ')' { PARENTHR (info lexbuf) }
  | _ { raise (SyntaxError ("Unexpected character: [" ^ Lexing.lexeme lexbuf ^ "]")) }
  | eof { EOF (info lexbuf) }
