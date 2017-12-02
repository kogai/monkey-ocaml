{
open Core
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
    Ast.create_info pos_fname pos_lnum (pos_cnum - pos_bol)

  let identifier lexbuf =
    Lexing.lexeme lexbuf
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9'] ['0'-'9']* 
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
(* let str = [^ '"' '\n'] [^ '"' '\n']* *)

rule read =
  parse
  | white { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | ';' { SEMICORON (info lexbuf) }
  | ':' { CORON (info lexbuf) }
  | '{' { BRACEL (info lexbuf) }
  | '}' { BRACER (info lexbuf) }
  | '=' { EQUAL (info lexbuf) }
  | '+' { PLUS (info lexbuf) }
  | '-' { MINUS (info lexbuf) }
  | '*' { STAR (info lexbuf) }
  | '.' { DOT (info lexbuf) }
  | '|' { BAR (info lexbuf) }
  | "==" { EQUAL_D (info lexbuf) }
  | "->" { ARROW (info lexbuf) }
  | "true" { BOOLEAN ((info lexbuf), true) }
  | "false" { BOOLEAN ((info lexbuf), false) }
  | "if" { IF (info lexbuf) }
  | "then" { THEN (info lexbuf) }
  | "else" { ELSE (info lexbuf) }
  | "case" { CASE (info lexbuf) }
  | "of" { OF (info lexbuf) }
  | "any" { ANY (info lexbuf) }
  | "()" { UNIT (info lexbuf) }
  | "(*" { comment lexbuf; read lexbuf }

  | "let" { LET (info lexbuf) }
  | "in" { IN (info lexbuf) }
  | "def" { DEF (info lexbuf) }

  | "bool" { TYPE_BOOLEAN (info lexbuf) }
  | "nat" { TYPE_NAT (info lexbuf) }
  | "unit" { TYPE_UNIT (info lexbuf) }

  | digit { NAT (
    (info lexbuf),
    (int_of_string (identifier lexbuf))
  )}
  | id { IDENTIFIER ((info lexbuf), (identifier lexbuf)) }
  | '(' { PARENTHL (info lexbuf) }
  | ')' { PARENTHR (info lexbuf) }
  | ',' { COMMA (info lexbuf) }
  | _ { raise (SyntaxError (sprintf "Unexpected character: [%s]" (Lexing.lexeme lexbuf))) }
  | eof { EOF (info lexbuf) }

and comment = parse
  | "*)" {}
  | [^ '\n'] { comment lexbuf }
  | "\n" { new_line lexbuf; comment lexbuf }
