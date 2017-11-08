
open Ast

exception TypeError of info * string

(* let from_env name env = match Eval.Environment.get name env with *)
(* () *)

(* let getTypeFromContext fi ctx i =
   match getbinding fi ctx i with
      VarBind(tyT) -> tyT
    | _ -> error fi 
      ("getTypeFromContext: Wrong kind of binding for variable " 
       ^ (index2name fi ctx i))  *)

