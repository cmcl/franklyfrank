(* A small test file for typing pattern matching compilation module. *)

open ErrorHandling
open Lexer
open Lexing
open ListUtils
open MidTranslate
open MidTree
open MidTyping
open ParseTree
open ParseTreeBuilder
open PatternMatching
open Printf
open Utility

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

(* Value pattern helper constructors *)
let vpat = Pattern.vpat
let any_vpat = Pattern.any_value
let any_pat = vpat @ any_vpat
let bool_pat = vpat @ Pattern.boolean
let ctr_pat k vs = vpat (Pattern.ctr k ~pats:vs ())
let int_pat = vpat @ Pattern.integer
let float_pat = vpat @ Pattern.float
let str_pat = vpat @ Pattern.str
let var_vpat = Pattern.var
let var_pat = vpat @ var_vpat

(* Value helper constructors *)
let ival iv = Mcvalue_ivalue iv
let bool_val b = ival (Mivalue_bool b)
let ctr_val k vs = Mcvalue_ctr (k, vs)
let id_val id = ival (Mivalue_var id)
let int_val n = ival (Mivalue_int n)
let float_val f = ival (Mivalue_float f)
let str_val s = ival (Mivalue_str s)

let make_iexp n = Mccomp_cvalue (Mcvalue_ivalue (Mivalue_int n))

(* Helper constructors for values. *)
let make_cons x xs = ctr_val "Cons" [x; xs]
let make_nil () = ctr_val "Nil" []
let make_unit () = ctr_val "Unit" []
let make_one () = make_cons (make_unit ()) (make_nil ())

let ps =
  [int_pat 1;
   bool_pat false;
   str_pat "Hello";
   var_pat "x";
   ctr_pat "Cons" [any_vpat (); var_vpat "xs"];
   float_pat 1.2;
   any_pat ()]

let vs =
  [int_val 1;
   bool_val false;
   str_val "Hello";
   id_val "y"; 
   ctr_val "Cons" [id_val "x"; ctr_val "Cons" [id_val "y"; id_val "ys"]];
   float_val 1.2;
   ctr_val "Unit" []]

let pa =
  [([ctr_pat "Nil" []; any_pat ()], make_iexp 1);
   ([any_pat (); ctr_pat "Nil" []], make_iexp 2);
   ([ctr_pat "Cons" [any_vpat (); var_vpat "xs"];
     ctr_pat "Cons" [any_vpat (); var_vpat "ys"]], make_iexp 3)]

let div =
  [([var_pat "x"; int_pat 0], make_iexp 1);
   ([var_pat "x"; var_pat "y"], make_iexp 2)]

let run_test p v =
  let msg = ShowPattern.show p ^ " <= " ^ ShowMidCValue.show v in
  print_endline (msg ^ " = " ^ (string_of_bool (is_inst p v)))

let run_match_test (km, m) (kvs, vs) =
  let msg = "matches " ^ kvs ^ " " ^ km ^ " = " in
  let res = matches vs m in
  print_endline (msg ^ Show.show<int option> res)

let gen_tests () =
  let vs1 = [make_nil (); make_one ()] in
  let vs2 = [make_one (); make_nil ()] in
  let vs3 = [make_one (); make_one ()] in
  [("vs1", vs1); ("vs2", vs2); ("vs3", vs3)]

let get_typing_env (name, prog) =
  let lexbuf = Lexing.from_string prog in
  let () = lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with pos_fname = name
  } in
  let (mtree, hmap, ctrs, cmds) = parse_file lexbuf in
  let (_, env) = type_with_error mtree in
  env

let test_list =
  String.concat ""
  ["data List x = Nil : List x | Cons : x -> List x -> List x.\n";
   "main : Int.\n";
   "main = 0.\n"]
  
let main =
  let env = get_typing_env ("test_list", test_list) in
  print_endline (Show.show<src_type> (env_lookup "List" env))
  
