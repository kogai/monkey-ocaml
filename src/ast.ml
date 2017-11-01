(* filename * line * column *)
type info = INFO of string * int * int
[@@deriving show]

let create_info file line column = INFO (file, line, column)

type t =
  | TermVar of info * string
  | TermAbs of info * string * t
  | TermApp of info * t * t
[@@deriving show]

let get_info = function
  | TermVar(i, _) -> i
  | TermAbs(i, _, _) -> i
  | TermApp(i, _, _) -> i
