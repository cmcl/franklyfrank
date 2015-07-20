open Lexer
open Lexing
open Printf
open ParseTree
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

let compose f g = fun x -> f (g x)

let rec pats_of_string ?(sep = "") ps =
  match ps with
  | [] -> ""
  | p :: _
    -> if List.length ps > 1 then List.fold_right (pat_of_string sep) ps ""
      else pat_of_string sep p ""

and vpat_of_string p =
  let
      to_pat = fun x -> {spat_desc = Spat_value x}
  in
  match p with
  | Svpat_var v -> v
  | Svpat_ctr (c,ps)
    -> "(" ^ c ^ " " ^ pats_of_string (List.map to_pat ps) ^ ")"

and cpat_of_string p = " " (* TODO: Add support *)

and pat_of_string sep p acc = (* Implementation assumes used for fold_right *)
  let pstr =
    match p.spat_desc with
    | Spat_value vp -> vpat_of_string vp
    | Spat_comp cp -> cpat_of_string cp
  in if acc = "" (* end of a sequence of patterns *) then pstr
     else pstr ^ sep ^ " " ^ acc

let rec cval_of_string cur cv =
  let cvstr = match cv with
              | CValue_ivalue iv -> ival_of_string iv
	      | CValue_ctr (c,vs)
		-> "(" ^ c ^ (List.fold_left cval_of_string "" vs) ^ ")"
	      | CValue_thunk c -> "{" ^ ccomp_of_string c ^ "}"
  in
  cur ^ " " ^ cvstr

and icomp_of_string ic =
  match ic with
  | IComp_force iv -> ival_of_string iv ^ "!"
  | IComp_app (ic, cc)
    -> "(" ^ icomp_of_string ic ^ " " ^ ccomp_of_string cc ^ ")"

and ival_of_string iv =
  match iv with
  | IValue_ident id -> id
  | IValue_icomp ic -> icomp_of_string ic

and ccomp_of_string cc =
  match cc with
  | CComp_cvalue cv -> cval_of_string "" cv
  | CComp_hdr_clause (ps, cc)
    -> pats_of_string ~sep:"," ps ^ " -> " ^ ccomp_of_string cc
  | CComp_emp_clause -> "()"
  | CComp_compose (c1,c2) -> ccomp_of_string c1 ^ " | " ^ ccomp_of_string c2

let print_def vd =
  printf "\t%s %s = %s\n" vd.vdef_name
    (pats_of_string vd.vdef_args)
    (ccomp_of_string vd.vdef_comp)

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

