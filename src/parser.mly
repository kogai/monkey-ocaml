%{
  open Ast
  open Core
%}

%token <Ast.info> LAMBDA
%token <Ast.info> ARROW
%token <Ast.info * string> IDENTIFIER
%token <Ast.info> PARENTHL
%token <Ast.info> PARENTHR
%token <Ast.info> EOF

%start <Ast.t option> program
%%

program:
  | EOF { None }
  | v = expression { Some v };
expression:
  | id = IDENTIFIER; { TermVar (Tuple2.get1 id, Tuple2.get2 id) }
  | LAMBDA; id = IDENTIFIER; ARROW tm = expression { TermAbs ($1, (Tuple2.get2 id), tm) }
  /* | e1 = expression; e2 = expression; { TermApp (get_info e1, e1, e2) } */
  /* | PARENTHL; t = expression; PARENTHR { t } */
