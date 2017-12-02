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
  | Variant of ty list
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
  | TermGet of info * t * string
  | TermDef of info * string * t
  | TermLet of info * string * t * t
  | TermCase of info * t * (ty * t) list
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
  | TermGet (i, _, _)
  | TermCase (i, _, _)
    -> i
  | TermLet (i, _, _, _)
  | TermAbs (i, _, _, _)
  | TermIf (i, _, _, _)
    -> i

let rec string_of_t = Printf.(function
    | TermUnit _ -> "()"
    | TermVar (_, name) -> name
    | TermNat (_, v) -> string_of_int v
    | TermBool (_, b) -> string_of_bool b
    | TermTuple (_, ts) -> ts
                           |> Core.List.map ~f:string_of_t
                           |> Core.String.concat ~sep:", "
                           |> sprintf "(%s)"
    | TermRecord (_, ts) -> ts
                            |> Core.List.map ~f:(fun (n, t) -> Printf.sprintf "%s: %s" n (string_of_t t))
                            |> Core.String.concat ~sep:", "
                            |> sprintf "{ %s }"
    | TermDef (_, n, t) -> sprintf "def %s = %s;" n (string_of_t t)
    | TermApp (_, t1, t2) -> sprintf "%s %s" (string_of_t t1) ((string_of_t t2))
    | TermGet (_, t, n) -> sprintf "%s.%s" (string_of_t t) n
    | TermLet (_, name, t1, t2) -> sprintf ""
    | TermAbs (_, name, t1, t2) -> sprintf ""
    | TermIf (_, name, t1, t2) -> sprintf ""
    | TermCase (_, c, ts) -> sprintf ""
  )

let rec string_of_type = function
  | Arrow (ty1, ty2) -> Printf.sprintf "%s -> %s" (string_of_type ty1) (string_of_type ty2)
  | Record ts -> ts
                 |> Core.List.map ~f:(fun (n, t) -> Printf.sprintf "%s: %s" n (string_of_type t))
                 |> Core.String.concat ~sep:" * "
                 |> Printf.sprintf "{ %s }"
  | Tuple ts
    -> ts
       |> Core.List.map ~f:string_of_type
       |> Core.String.concat ~sep:" * "
       |> Printf.sprintf "(%s)"
  | Variant ts
    -> ts
       |> Core.List.map ~f:string_of_type
       |> Core.String.concat ~sep:" + "
       |> Printf.sprintf "(%s)"
  | Nat -> "nat"
  | Boolean -> "bool"
  | Unit -> "unit"
