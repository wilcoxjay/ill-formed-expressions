
let usage_msg =
"Main <file>

OPTIONS:
  -h --help         print this usage information and exit
"

let usage () =
  print_endline usage_msg

let file = ref ""

let parse_args () =
  let _file : string option ref = ref None in
  let bogus_arg x =
    usage ();
    failwith ("Main: bogus arg: " ^ x)
  in
  let check_file () =
    match !_file with
    | None -> usage (); failwith ("Main: filename is required")
    | Some f -> file := f
  in
  let check_required () =
    check_file ()
  in
  let rec loop = function
    | [] -> check_required ()
    | "-h" :: rest
    | "--help" :: rest ->
       usage ();
       Pervasives.exit 0
    | x :: rest ->
       if Util.startswith "-" x then
         bogus_arg x
       else
         _file := Some x;
       loop rest
  in
  Sys.argv
    |> Array.to_list
    |> List.tl
    |> loop


let lexer lexbuf =
  ILLPL.Lexer.token lexbuf

(* uncomment to log token stream *)
(* useful for debugging parser/lexer *)
(*
let lexer lexbuf =
  let res = lexer lexbuf in
  Printf.printf "Saw token [%s]\n%!" (Lexing.lexeme lexbuf);
  res
 *)

let main () =
  parse_args ();
  Parser.of_file ILLPL.Parser.prog lexer !file
    |> Format.printf "%a\n%!" ILLPL.Printer.pr_prog


let _ = main ()
