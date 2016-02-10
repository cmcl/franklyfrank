(** Untyped Frank Evaluator *)
open Lexer
open Lexing
open Printf
open ParseTree
open MidTree
open MidTranslate
open MidTyping
open MidEvaluator
open ErrorHandling

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

let translate_with_error prog =
  let ext = function  Merr_not_comp msg
                    | Merr_inv_clause msg
                    | Merr_inv_ctr msg
                    | Merr_no_main msg
                    | Merr_duplicate_tvar msg
                    | Merr_shadowing_builtin msg -> msg in
  try translate prog with
  | MidTranslate.Error err
    -> fprintf stderr "Translation error: %s\n" (ext err);
      exit (-1)

let type_with_error prog =
  try type_prog prog with
  | TypeError s -> fprintf stderr "Type error: %s\n" s; exit (-1)

let rec parse_file lexbuf =
  match parse_with_error lexbuf with
  | [] -> ([], HandlerMap.empty, CtrSet.empty, CmdSet.empty)
  | prog -> translate_with_error prog

let preprocess_lines inx =
  let last buf = Buffer.length buf - 1 in
  let nth buf n = Buffer.nth buf n in
  (* Make it optional to include the dot at the end of a sentence and
     also guard against special cases e.g. line ends in a comment or we
     encountered a blank line. *)
  let last_cond buf =
    nth buf (last buf) != '.' &&
    (nth buf ((last buf) - 1) != '-' && nth buf (last buf) != '}') &&
    nth buf (last buf) != '\n' in
  let rec process_char_until c buf =
    let c' = input_char inx in
    if c' = c then c
    else if c' = '{' then (* Multi-line comment encountered. *)
      let d = input_char inx in
      Buffer.add_char buf c'; Buffer.add_char buf d;
      (* Eat entire comment. *)
      (if d = '-' then Buffer.add_char buf (process_char_until '}' buf));
      process_char_until c buf
    else if c' = '"' then (* String encountered. *)
      (Buffer.add_char buf c';
       (* Eat entire string. *)
       Buffer.add_char buf (process_char_until '"' buf);
       process_char_until c buf)
    else (Buffer.add_char buf c'; process_char_until c buf) in
  let rec process_lines buf =
    let nl = process_char_until '\n' buf in
    let c = try input_char inx with
      | End_of_file -> '\n' in
    (if c != ' ' && c != '\t' && last_cond buf then
	Buffer.add_char buf '.');
    Buffer.add_char buf nl; Buffer.add_char buf c;
    process_lines buf in
  let buf = Buffer.create 10 in
  try process_lines buf with
  | End_of_file -> Buffer.contents buf

let loop filename =
  let inx = open_in filename in
  let buf = preprocess_lines inx in
  let lexbuf = Lexing.from_string buf in
  let () = lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = filename
  } in
  let (mtree, hmap, ctrs, cmds) = parse_file lexbuf in
  Debug.print "%s" (ShowMidProg.show mtree);
  let (t, env) = type_with_error mtree in
  Debug.print "Program typechecked with main : %s" (ShowSrcType.show t);
  let res = EvalComp.eval env hmap mtree in
  Debug.print "%s\n" (EvalComp.show res);
  close_in inx

let () =
  let flags = [("-debug",
		Arg.Unit (fun () -> Debug.debug_flag true),
                "Enable debugging information")] in
  Arg.parse flags loop "Frank Parser:"

