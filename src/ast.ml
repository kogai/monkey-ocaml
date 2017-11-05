(* filename * line * column *)
type info = string * int * int
[@@deriving show]

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column
let create_info file line column = (file, line, column)

type t =
  | TermVar of info * string
  | TermAbs of info * string * t
  | TermApp of info * t * t
  | TermIf of info * t * t * t
  | TermBool of info * bool
[@@deriving show]

let get_info = function
  | TermVar (i, _) 
  | TermBool (i, _) -> i
  | TermAbs (i, _, _)
  | TermApp (i, _, _) -> i
  | TermIf (i, _, _, _) -> i
