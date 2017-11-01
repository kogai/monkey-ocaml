open Core
open Lexing

let rec parse lexbuf =
  match Parser.program Lexer.read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)
(* 
let rec is_num = Ast.(function
    | TermZero _ -> true
    | TermSucc (_, num) -> is_num num
    | _ -> false) *)

let rec eval' = Ast.(function
    (* | TermSucc (info, t) -> TermSucc (info, eval' t)

    | TermPred (info, TermZero _) -> TermZero info
    | TermPred (info, TermSucc (_, num)) when is_num num -> num
    | TermPred (info, t) -> TermPred (info, eval' t)

    | TermIsZero (info, TermZero _) -> TermTrue info
    | TermIsZero (info, TermSucc (_, num)) when is_num num -> TermFalse info
    | TermIsZero (info, t) -> eval' @@ TermIsZero (info, eval' t)

    | TermIf (_, TermTrue _, t2, t3) -> t2
    | TermIf (_, TermFalse _, t2, t3) -> t3
    | TermIf (info, t1, t2, t3) -> eval' @@ TermIf (info, eval' t1, t2, t3) *)

    (* No rule to applies *)
    (* | TermTrue info
    | TermFalse info
    | TermZero info as t -> t *)
    | t -> t
  )

let eval filename lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  List.map ~f:eval' (parse lexbuf)
