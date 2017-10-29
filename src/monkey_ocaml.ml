open Core
open Lexer
open Lexing

let rec parse lexbuf =
  match Parser.program read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let print statements =
  statements
  |> List.map ~f:Ast.show
  |> String.concat ~sep:"\n"
  |> (fun ast ->
      Out_channel.write_all "fixture/arith.dump" ast
    )

let filename = "fixture/arith.mky"
let () =
  print_endline filename;
  filename
  |> In_channel.create
  |> Lexing.from_channel
  |> parse
  |> print
