open Core
open Lexer
open Lexing

let parse lexbuf =
  let result = try Parser.program read lexbuf with
    | SyntaxError reason ->
      fprintf stderr "[HERE]: \n%s" reason;
      exit 1
  in
  let result = (match result with
      | Some ast -> Ast.show ast
      | None -> Ast.show `TermIllegal
    ) in
  Printf.printf "%s\n" result;
  ()

let filename = "fixture/arith.mky"
let () =
  print_endline filename;
  filename
  |> In_channel.create
  |> Lexing.from_channel
  |> parse
