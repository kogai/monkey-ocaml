type t =
  | Arrow of t * t
  | Boolean
[@@deriving show]
