%{
  open Ast
  open Core
%}
%token <Ast.info> LAMBDA
%token <Ast.info> ARROW
%token <Ast.info * string> IDENTIFIER
%token <Ast.info> PARENTHL
%token <Ast.info> PARENTHR
%token <Ast.info> TERMINATE
%token <Ast.info> EOF
%start <Ast.t option> program

%%

program:
  | EOF { None }
  | v = term TERMINATE { Some v }
term:
  | t = app_term { t }
  | LAMBDA; id = IDENTIFIER ARROW tm = term { TermAbs ($1, (Tuple2.get2 id), tm) }
app_term:
  | t = atom_term { t }
  | e1 = app_term e2 = atom_term { TermApp (get_info e1, e1, e2) }
atom_term:
  | PARENTHL t = term PARENTHR { t }
  | id = IDENTIFIER { TermVar (Tuple2.get1 id, Tuple2.get2 id) }
