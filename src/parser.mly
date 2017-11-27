%{
  open Ast
  open Core
%}
%token <Ast.info> ARROW
%token <Ast.info> IF
%token <Ast.info> THEN
%token <Ast.info> ELSE
%token <Ast.info> CASE
%token <Ast.info> OF
%token <Ast.info> BAR

%token <Ast.info> TYPE_BOOLEAN
%token <Ast.info> TYPE_NAT
%token <Ast.info> TYPE_UNIT

%token <Ast.info * string> IDENTIFIER
%token <Ast.info * bool> BOOLEAN
%token <Ast.info * int> NAT
%token <Ast.info> UNIT

%token <Ast.info> LET
%token <Ast.info> IN
%token <Ast.info> DEF

%token <Ast.info> EQUAL
%token <Ast.info> PLUS

%token <Ast.info> PARENTHL
%token <Ast.info> PARENTHR
%token <Ast.info> BRACEL
%token <Ast.info> BRACER
%token <Ast.info> SEMICORON
%token <Ast.info> CORON
%token <Ast.info> COMMA
%token <Ast.info> STAR
%token <Ast.info> DOT
%token <Ast.info> EOF
%start <Ast.t option> program

%%

program:
  | EOF { None }
  | v = term SEMICORON { Some v }

term:
  | DEF id = IDENTIFIER EQUAL t = term { TermDef (Tuple2.get1 id, Tuple2.get2 id, t) }
  | PARENTHL
    id = IDENTIFIER
    CORON
    ty = typing
    PARENTHR
    ARROW
    tm = term
    { TermAbs ((Tuple2.get1 id), (Tuple2.get2 id), ty, tm) }
  | t = app_term { t }
app_term:
  | t = atom_term { t }
  | e1 = app_term e2 = atom_term { TermApp (get_info e1, e1, e2) }
atom_term:
  | IF c = term THEN t1 = term ELSE t2 = term { TermIf ((get_info c), c, t1, t2) }
  | i = CASE c = term OF BAR arms = separated_list(BAR, case_arm) { TermCase (i, c, arms) }
  | t = atom_term info = DOT id = IDENTIFIER { TermGet (info, t, Tuple2.get2 id) }
  | t = atom_term info = DOT idx = NAT { TermGet (info, t, (string_of_int (Tuple2.get2 idx))) }
  | LET id = IDENTIFIER EQUAL t1 = term IN t2 = term { TermLet (Tuple2.get1 id, Tuple2.get2 id, t1, t2) }
  | i = BRACEL t = separated_list(COMMA, record_field) BRACER { TermRecord (i, t) }
  | i = PARENTHL t = separated_list(COMMA, term) PARENTHR { TermTuple (i, t) }
  | PARENTHL t = term PARENTHR { t }
  | id = IDENTIFIER { TermVar (Tuple2.get1 id, Tuple2.get2 id) }
  | n = NAT { TermNat (Tuple2.get1 n, Tuple2.get2 n) }
  | b = BOOLEAN { TermBool (Tuple2.get1 b, Tuple2.get2 b) }
  | u = UNIT { TermUnit u }
record_field:
  | label = IDENTIFIER CORON value = term { (Tuple2.get2 label, value) }
case_arm:
  | ty = atom_typing ARROW tm = term { (ty, tm) }

typing:
  | t = arrow_typing { t }
arrow_typing:
  | t1 = atom_typing ARROW t2 = arrow_typing { Ast.Arrow (t1, t2) }
  | t = atom_typing { t }
atom_typing:
  | BRACEL t = separated_list(STAR, record_field_typing) BRACER { Record t }
  | PARENTHL t = separated_list(STAR, typing) PARENTHR { Tuple t }
  | PARENTHL t = separated_list(PLUS, typing) PARENTHR { Variant t }
  | PARENTHL t = typing PARENTHR { t }
  | TYPE_BOOLEAN { Ast.Boolean }
  | TYPE_NAT { Ast.Nat }
  | TYPE_UNIT { Ast.Unit }
record_field_typing:
  | label = IDENTIFIER CORON value = typing { (Tuple2.get2 label, value) }
