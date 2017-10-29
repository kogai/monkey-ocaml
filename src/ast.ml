(*  *)
type info = INFO of string * int * int | UNKNOWN

type t = [
  | `TermTrue
  | `TermFalse
  | `TermZero
  | `TermSucc of t
  | `TermPred of t
  | `TermIsZero of t
  | `TermIf of t * t * t
  | `TermIllegal
]
[@@deriving show]
