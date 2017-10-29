%token TRUE
%token FALSE
%token ZERO
%token ISZERO
%token SUCC
%token PRED
%token PARENTHL
%token PARENTHR
%token IF
%token THEN
%token ELSE
%token EOF

%start <Ast.t option> program
%%

program:
  | EOF { None }
  | v = expression { Some v };
expression:
  | IF; t1 = expression; THEN; t2 = expression; ELSE t3 = expression { `TermIf (t1, t2, t3) }
  | PARENTHL; t = expression; PARENTHR { t }
  | ISZERO; t = expression { `TermIsZero t }
  | SUCC; t = expression { `TermSucc t }
  | PRED; t = expression { `TermPred t }
  | TRUE { `TermTrue }
  | FALSE { `TermFalse }
  | ZERO { `TermZero }
