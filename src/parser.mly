%token <Ast.info> TRUE
%token <Ast.info> FALSE
%token <Ast.info> ZERO
%token <Ast.info> ISZERO
%token <Ast.info> SUCC
%token <Ast.info> PRED
%token <Ast.info> PARENTHL
%token <Ast.info> PARENTHR
%token <Ast.info> IF
%token <Ast.info> THEN
%token <Ast.info> ELSE
%token <Ast.info> EOF

%start <Ast.t option> program
%%

program:
  | EOF { None }
  | v = expression { Some v };
expression:
  | IF; t1 = expression; THEN; t2 = expression; ELSE t3 = expression { `TermIf ($1, t1, t2, t3) }
  | PARENTHL; t = expression; PARENTHR { t }
  | ISZERO; t = expression { `TermIsZero ($1, t) }
  | SUCC; t = expression { `TermSucc ($1, t) }
  | PRED; t = expression { `TermPred ($1, t) }
  | TRUE { `TermTrue ($1)}
  | FALSE { `TermFalse ($1)}
  | ZERO { `TermZero ($1) }
