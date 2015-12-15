(* A small test file for the pattern matching compilation module. *)

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

let run_test p v =
  let msg = ShowPattern.show p ^ " <= " ^ ShowMidCValue.show v in
  print_endline (msg ^ " = " ^ (string_of_bool (is_inst p v)))

let main =
  List.map (uncurry run_test) (List.combine ps vs)
