%{
  open Ast
  open Core
%}
%token <Ast.info> ARROW
%token <Ast.info> IF
%token <Ast.info> THEN
%token <Ast.info> ELSE
%token <Ast.info> TYPE_BOOLEAN
%token <Ast.info> TYPE_NAT

%token <Ast.info * string> IDENTIFIER
%token <Ast.info * bool> BOOLEAN
%token <Ast.info * int> NAT

%token <Ast.info> PARENTHL
%token <Ast.info> PARENTHR
%token <Ast.info> SEMICORON
%token <Ast.info> CORON
%token <Ast.info> EOF
%start <Ast.t option> program

%%

program:
  | EOF { None }
  | v = term SEMICORON { Some v }

term:
  | t = app_term { t }
  | IF c = term THEN t1 = term ELSE t2 = term { TermIf ((get_info c), c, t1, t2) }
  | PARENTHL
    id = IDENTIFIER
    CORON
    ty = typing
    PARENTHR
    ARROW
    tm = term
    { TermAbs ((Tuple2.get1 id), (Tuple2.get2 id), ty, tm) }
app_term:
  | t = atom_term { t }
  | e1 = app_term e2 = atom_term { TermApp (get_info e1, e1, e2) }
atom_term:
  | PARENTHL t = term PARENTHR { t }
  | id = IDENTIFIER { TermVar (Tuple2.get1 id, Tuple2.get2 id) }
  | n = NAT { TermNat (Tuple2.get1 n, Tuple2.get2 n) }
  | b = BOOLEAN { TermBool (Tuple2.get1 b, Tuple2.get2 b) }

typing:
  | t = arrow_typing { t }
arrow_typing:
  | t1 = atom_typing ARROW t2 = arrow_typing { Ast.Arrow (t1, t2) }
  | t = atom_typing { t }
atom_typing:
  | PARENTHL t = typing PARENTHR { t }
  | TYPE_BOOLEAN { Ast.Boolean }
  | TYPE_NAT { Ast.Nat }
