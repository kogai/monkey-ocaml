open Core
open Lexer
open Lexing

exception Unreachable

let to_string statements =
  statements
  |> List.map ~f:Ast.show
  |> String.concat ~sep:"\n"

let write path content =
  let filename = match Filename.split_extension path with
    | filename, Some _ -> sprintf "%s.dump" filename
    | _ -> raise Unreachable
  in
  print_endline content;
  Out_channel.write_all filename content

let run filename () =
  filename
  |> In_channel.create
  |> Lexing.from_channel
  |> Eval.eval filename Eval.Environment.empty
  |> to_string
  |> write filename

let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    run
  |> Command.run
