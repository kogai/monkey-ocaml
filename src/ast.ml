(* filename * line * column *)
type info = INFO of string * int * int
[@@deriving show]

let createInfo file line column = INFO (file, line, column)

type t =
  | TermVar of info
  | TermAbs of info
  | TermApp of info
[@@deriving show]
