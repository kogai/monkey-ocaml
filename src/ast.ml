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
  | Tuple of ty list
  | Record of (string * ty) list
[@@deriving show]

type t =
  | TermVar of info * string
  | TermAbs of info * string * ty * t
  | TermApp of info * t * t
  | TermIf of info * t * t * t
  | TermBool of info * bool
  | TermNat of info * int
  | TermUnit of info
  | TermTuple of info * t list
  | TermRecord of info * (string * t) list
  | TermDef of info * string * t
  | TermLet of info * string * t * t
[@@deriving show]

let get_info = function
  | TermUnit i -> i
  | TermVar (i, _) 
  | TermNat (i, _)
  | TermBool (i, _)
  | TermTuple (i, _)
  | TermRecord (i, _)
    -> i
  | TermDef (i, _, _)
  | TermApp (i, _, _)
    -> i
  | TermLet (i, _, _, _)
  | TermAbs (i, _, _, _)
  | TermIf (i, _, _, _)
    -> i

let rec string_of_type = function
  | Arrow (ty1, ty2) -> Printf.sprintf "%s -> %s" (string_of_type ty1) (string_of_type ty2)
  | Record ts -> ts
                 |> Core.List.map ~f:(fun (n, t) -> Printf.sprintf "%s: %s" n (string_of_type t))
                 |> Core.String.concat ~sep:" * "
                 |> Printf.sprintf "{ %s }"
  | Tuple ts -> ts
                |> Core.List.map ~f:string_of_type
                |> Core.String.concat ~sep:" * "
                |> Printf.sprintf "(%s)"

  | Nat -> "nat"
  | Boolean -> "bool"
  | Unit -> "()"
