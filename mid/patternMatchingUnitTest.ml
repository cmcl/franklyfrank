(* A small test file for the pattern matching compilation module. *)

open ListUtils
open MidTree
open ParseTree
open ParseTreeBuilder
open PatternMatching
open Utility

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

let main =
  List.iter (uncurry run_test) (List.combine ps vs);
  print_endline "----P -> A------";
  prmatrix pa;
  print_endline "----S((::), P->A)-----Specialising to Cons----";
  prmatrix (specialise (MidTyping.TSCtr ("Cons", 2)) pa);
  print_endline "----S([], P->A)-----Specialising to Nil----";
  prmatrix (specialise (MidTyping.TSCtr ("Nil", 0)) pa);
  let pm = get_pmatrix pa in
  print_endline "---just the patterns----";
  prpatmatrix pm;
  List.iter (run_match_test ("P", pm)) (gen_tests ());
  print_endline "---div---";
  prmatrix div;
  print_endline "---S(0, swap div 0 1)---Specalising to error case---";
  let (pm,rs) = to_columns div in
  let pm' = swap pm 0 1 in
  let div' = List.combine (transpose pm') rs in
  prmatrix (specialise (MidTyping.TSInt 0) div')
