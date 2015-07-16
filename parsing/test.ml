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

let compose f g = fun x -> f (g x)

let rec print_vpat p =
  let
      ptf = compose print_pattern (fun x -> {spat_desc = Spat_value x})
  in
  match p with
  | Svpat_var v -> printf " %s" v
  | Svpat_ctr (c,ps)
    -> printf " (%s" c; ignore (List.map ptf ps); printf ")"

and print_cpat p = printf " "

and print_pattern p =
  match p.spat_desc with
  | Spat_value vp -> print_vpat vp
  | Spat_comp cp -> print_cpat cp

let rec print_cval cv =
  match cv with
  | CValue_ivalue iv -> print_ival iv
  | CValue_ctr (c,vs)
    -> printf " (%s" c; ignore (List.map print_cval vs); printf ")"
  | CValue_thunk c -> printf " {"; print_comp c; printf " }"

and print_ival iv =
  match iv with
  | IValue_ident id -> printf "%s" id
  | IValue_icomp ic -> print_icomp ic

and print_icomp ic =
  match ic with
  | IComp_force iv -> print_ival iv; printf "!"
  | IComp_app (ic, cc)
    -> printf "("; print_icomp ic; printf " "; print_comp cc;
       printf ")"

and print_comp comp =
  match comp with
  | CComp_cvalue cv -> print_cval cv
  | CComp_hdr_clause (ps, cc)
    -> ignore (List.map
		 (fun p -> printf " "; print_pattern p; printf ",")
		 ps);
       printf " ->";
       print_comp cc
  | CComp_emp_clause -> printf " ()"
  | CComp_compose (c1, c2) -> print_comp c1; print_comp c2

let print_def vd =
  print_endline "\t";
  printf "%s" vd.vdef_name;
  ignore (List.map print_pattern vd.vdef_args);
  printf " =";
  print_comp vd.vdef_comp;
  printf "\n"

let print_term trm = 
  match trm with
  | Sterm_datatype dt -> printf "Datatype\t%s\n" dt.sdt_name
  | Sterm_effin ei -> printf "Effect Interface\t%s\n" ei.sei_name
  | Sterm_vdecl vd -> printf "Value Declaration\t%s\n" vd.svdecl_name
  | Sterm_vdefn vdef
    -> printf "Value Definition\t%s:\n" vdef.vdef_name; print_def vdef

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

