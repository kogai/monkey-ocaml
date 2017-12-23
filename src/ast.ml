(* filename * line * column *)
type info = string * int * int
[@@deriving show]

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column

let create_info file line column = (file, line, column)

type t =
  | TermVar of info * string
  | TermAbs of info * string * t
  | TermApp of info * t * t

let rec show = Printf.(function
    | TermVar (_, name) -> sprintf "%s" name
    | TermApp (_, t1, t2) -> sprintf "(%s %s)" (show t1) ((show t2))
    | TermAbs (_, name, t) -> sprintf "(%s -> %s)" name (show t))

let get_info = function
  | TermVar(i, _) -> i
  | TermAbs(i, _, _) -> i
  | TermApp(i, _, _) -> i
