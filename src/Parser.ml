open Util.Combinators
open Lexing

exception ParseError of string
exception LexicalError of string

let set_fname path lexbuf =
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_fname = path }

let string_of_lexpos lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    pos.pos_fname
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let parse_aux parse lex lexbuf =
  try parse lex lexbuf with
  | LexicalError msg ->
     raise (LexicalError (Printf.sprintf "%s: %s"
        (string_of_lexpos lexbuf)
        msg))
  | ParseError msg ->
     raise (ParseError (Printf.sprintf "%s: %s"
        (string_of_lexpos lexbuf)
        msg))
  | e ->
     raise (ParseError (Printf.sprintf "%s: unexpected exception while parsing\n%s\n"
        (string_of_lexpos lexbuf)
        (Printexc.to_string e)))

let of_file parse lex path =
  Util.with_ic path (fun ic ->
    ic |> Lexing.from_channel
       >> set_fname path
       |> parse_aux parse lex)

let of_string parse lex s =
    s |> Lexing.from_string
      >> set_fname "(input)"
      |> parse_aux parse lex
