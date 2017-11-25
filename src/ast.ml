(* filename * line * column *)
type info = string * int * int
[@@deriving show]

let show_info (file, line, column) = Core.sprintf "%s:%d:%d" file line column
let create_info file line column = (file, line, column)

type ty =
  | Arrow of ty * ty
  | Nat
  | Boolean
  | Unit
[@@deriving show]

type t =
  | TermVar of info * string
  | TermAbs of info * string * ty * t
  | TermApp of info * t * t
  | TermIf of info * t * t * t
  | TermBool of info * bool
  | TermNat of info * int
  | TermUnit of info
[@@deriving show]

let get_info = function
  | TermUnit (i) -> i
  | TermVar (i, _) 
  | TermNat (i, _)
  | TermBool (i, _)
    -> i
  | TermApp (i, _, _)
    -> i
  | TermAbs (i, _, _, _)
  | TermIf (i, _, _, _)
    -> i

let rec string_of_type = function
  | Arrow (ty1, ty2) -> Printf.sprintf "%s -> %s" (string_of_type ty1) (string_of_type ty2)
  | Nat -> "nat"
  | Boolean -> "bool"
  | Unit -> "()"
