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
  let ext = function  Merr_not_comp msg -> msg
                    | Merr_inv_clause msg -> msg
                    | Merr_inv_ctr msg -> msg
                    | Merr_no_main msg -> msg in
  try translate prog with
  | MidTranslate.Error err
    -> fprintf stderr "Translation error: %s\n" (ext err);
      exit (-1)

let rec parse_file lexbuf =
  match parse_with_error lexbuf with
  | [] -> ([], HandlerMap.empty, CtrSet.empty, CmdSet.empty)
  | prog -> translate_with_error prog

let loop filename =
  let inx = open_in filename in
  let lexbuf = Lexing.from_channel inx in
  let () = lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = filename
  } in
  let (mtree, hmap, ctrs, cmds) = parse_file lexbuf in
  Debug.debug_flag true;
  Debug.print "%s" (ShowMidProg.show mtree);
  let t = type_prog mtree in
  Debug.print "Program typechecked with main : %s" (ShowSrcType.show t);
  let res = EvalComp.eval hmap mtree in
  Debug.print "%s\n" (EvalComp.show res);
  close_in inx

let () = Arg.parse [] loop "Frank Parser:"

