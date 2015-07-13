open Lexer
open Lexing
open Printf
open ParseTree

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.program Lexer.token lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a:%s\n" print_position lexbuf msg; []
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let print_term trm = 
  match trm with
  | Sterm_datatype dt -> printf "Datatype\t%s\n" dt.sdt_name
  | Sterm_effin ei -> printf "Effect Interface\t%s\n" ei.sei_name
  | Sterm_vdecl vd -> printf "Value Declaration\t%s\n" vd.svdecl_name
  | _ -> printf "Recognised but not handled by parser\n"


let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | [] -> ()
  | ts -> let _ = List.map print_term ts in parse_and_print lexbuf

let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print lexbuf;
  close_in inx

let () = Arg.parse [] loop "Frank Parser:"

