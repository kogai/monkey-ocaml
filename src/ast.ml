(* filename * line * column *)
type info = string * int * int
[@@deriving show]

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column

let create_info file line column = (file, line, column)

type t =
  | TermVar of info * string
  | TermAbs of info * string * t
  | TermApp of info * t * t
[@@deriving show]

let get_info = function
  | TermVar(i, _) -> i
  | TermAbs(i, _, _) -> i
  | TermApp(i, _, _) -> i
