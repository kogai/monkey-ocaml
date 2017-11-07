
open Ast

exception TypeError of info * string

let rec typeof' env = function
  | TermVar (info, name) -> raise @@ TypeError (info, "")
  | TermAbs (info, name, ty, term) -> raise @@ TypeError (info, "")
  | TermApp (info, term1, term2) -> raise @@ TypeError (info, "")
  | TermIf (info, condition, term1, term2) -> raise @@ TypeError (info, "")
  | TermBool (info, value) -> raise @@ TypeError (info, "")

let typeof env ast =
  typeof' env ast
