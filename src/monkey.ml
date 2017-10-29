open Core
open Lexer
open Lexing

exception Unreachable

let rec parse lexbuf =
  match Parser.program read lexbuf with
  | None -> []
  | Some statement -> statement::(parse lexbuf)

let print statements =
  statements
  |> List.map ~f:Ast.show
  |> String.concat ~sep:"\n"

let write path content =
  let filename = match Filename.split_extension path with
    | filename, Some _ -> sprintf "%s.dump" filename
    | _ -> raise Unreachable
  in
  Out_channel.write_all filename content

let run filename () =
  filename
  |> In_channel.create
  |> Lexing.from_channel
  |> parse
  |> print
  |> write filename

let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    run
  |> Command.run
