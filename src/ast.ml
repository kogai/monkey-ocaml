(* filename * line * column *)
type info = INFO of string * int * int | UNKNOWN
[@@deriving show]

let dummyinfo = UNKNOWN
let createInfo file line column = INFO (file, line, column)

type t =
  | TermTrue of info
  | TermFalse of info
  | TermZero of info
  | TermSucc of info * t
  | TermPred of info * t
  | TermIsZero of info * t
  | TermIf of info * t * t * t
[@@deriving show]
