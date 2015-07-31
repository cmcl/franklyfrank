(***********************************************************************
 * Evaluate the mid-level language.
 *
 *
 * Created by Craig McLaughlin on 21/07/2015.
 ***********************************************************************
 *)

open MidTranslate
open MidTree
open Monad
open ParseTree
open Printf

module type EVALCOMP = sig
  include MONAD

  type comp = value t
  and value =
    | VBool of bool
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  val (>=>) : (value -> 'a t) -> ('a -> 'b t) -> value -> 'b t
  val sequence : ('a t) list -> ('a list) t
  val command : string -> value list -> comp
  val show : comp -> string
  val vshow : value -> string

  val eval : MidTranslate.HandlerMap.mt -> MidTree.prog -> comp
end

module EvalComp : EVALCOMP = struct
  module ENV = Map.Make(String)

  exception UserDefShadowingBuiltin of string

  type 'a t =
    | Command of string * value list * (value -> 'a t)
    | Return of 'a

  and comp = value t

  and value =
    | VBool of bool
    | VInt of int
    | VCon of string * value list
    | VMultiHandler of (comp list -> comp)

  (** Monadic operations *)
  let return v = Return v

  let rec (>=>) f g x = f x >>= g
  and (>>=) m k =
    match m with
    | Command (c, vs, r) -> Command (c, vs, r >=> k)
    | Return v           -> k v

  let lift : (value -> comp) -> value = fun f ->
    VMultiHandler (fun [m] -> m >>= f)

  let rec sequence = function
    | []      -> return []
    | m :: ms -> m >>= (fun x -> sequence ms >>= (fun xs -> return (x :: xs)))

  let command c vs = Command (c, vs, return)

  let rec vshow v =
    match v with
    | VBool b -> string_of_bool b
    | VInt n -> string_of_int n
    | VCon (k, vs) -> "(" ^ k ^ (string_of_args " " vshow vs) ^ ")"
    | VMultiHandler _ -> "MULTIHANDLER"

  let show c =
    match c with
    | Command (c, vs, _)
      -> "Command (" ^ c ^ (string_of_args " " vshow vs) ^ ")"
    | Return v
      -> "Return (" ^ vshow v ^ ")"

  (* Return v is matched by x
   * Return (suc v) is matched by suc x and x *)
  let rec match_value (v, p) m =
    if not m then m
    else
      match v, p with
      | _, Svpat_var x -> true
      | VBool b, Svpat_bool b' -> b = b'
      | VInt n, Svpat_int n' -> n = n'
      | VCon (k, vs), Svpat_ctr (k', vs')
	-> k = k' && List.length vs = List.length vs' &&
           (List.fold_right match_value (List.combine vs vs') true)
      | _ -> false

  (* Command ("put", [v], r) is matched by [put x -> k] and [?c -> k] and
     [t] *)
  let match_command (c, vs, r) p =
    match p with
    | Scpat_request (c', vs', _)
      -> c = c' && List.length vs = List.length vs' &&
           (List.fold_right match_value (List.combine vs vs') true)
    | _ -> false

  let match_pair (c, p) m =
    if not m then m
    else
      match c, p.spat_desc with
      | Command (c', vs, r), Spat_comp cp -> match_command (c', vs, r) cp
      | Return v, Spat_value vp
	-> let res = match_value (v, vp) m in
	   Debug.print "Matching %s with %s...%s\n"
	     (vshow v) (ShowPattern.show p) (string_of_bool res); res
      | _ -> Debug.print "Whoa! No matches: %s with %s\n" (show c)
	             (ShowPattern.show p); false

  let pat_matches cs ps =
    if List.length cs > List.length ps then
      raise (invalid_arg "too many arguments")
    else if List.length cs < List.length ps then
      raise (invalid_arg "too few arguments")
    else
      List.fold_right match_pair (List.combine cs ps) true

  let just_hdrs = function Mtld_handler hdr -> Some hdr | _ -> None

  let disjoint hmap (n, _) =
    match HandlerMap.mem n hmap with
    | true -> raise (UserDefShadowingBuiltin n)
    | _ -> ()

  (** Builtin functions *)
  let gtdef env [cx; cy] = cx >>=
    function (VInt x) -> cy >>=
      (function (VInt y) -> return (VBool (x > y))
               | _ as vy -> invalid_arg ("second_arg:" ^ vshow vy))
            | _ as vx -> invalid_arg ("first arg:" ^ vshow vx)

  (** Create the builtin environment. *)
  let get_builtins () =
    let blts = [("gt", gtdef)] in
    let add_blt (n,d) env = ENV.add n d env in
    List.fold_right add_blt blts ENV.empty

  let rec eval hmap prog =
    let blt_env = get_builtins () in
    let hdrs = List.map (fun (k, h) -> h) (HandlerMap.bindings hmap) in
    let pre_env = List.fold_right construct_env_entry hdrs blt_env in
    let rec env' =
      lazy (ENV.map (fun f ->
	return (VMultiHandler (fun cs -> f (Lazy.force env') cs))) pre_env) in
    let env = Lazy.force env' in
    let main = ENV.find "main" env in
    main >>= function VMultiHandler f -> f [] | _ -> not_hdr ~desc:"main" ()

  and construct_env_entry hdr env =
    if ENV.mem hdr.mhdr_name env then
      raise (UserDefShadowingBuiltin hdr.mhdr_name)
    else
      ENV.add hdr.mhdr_name (fun env cs ->
	Debug.print "Evaluating handler %s...\n" hdr.mhdr_name;
	eval_tlhdrs env hdr.mhdr_defs cs) env

  and eval_tlhdrs env cls cs =
    List.fold_right (eval_clause env cs) cls pat_match_fail

  and eval_clause env cs (ps, cc) acc =
    Debug.print "%s with %s\n" (string_of_args ", " ~bbegin:false show cs)
      (string_of_args ", " ~bbegin:false ShowPattern.show ps);
    if pat_matches cs ps then eval_ccomp (extend env cs ps) cc else acc

  and extend env cs ps = env

  and pat_match_fail = command "PatternMatchFail" []

  and app_if_ne s t =
    s ^ (if t <> "" then " : " ^ t else t)

  and unhandled_comp ?(desc = "") () =
    command (app_if_ne "UnhandledComputation" desc) []

  and not_hdr ?(desc = "") () =
    command (app_if_ne "NotAHandler" desc) []

  and eval_ccomp env cc =
    match cc with
    | Mccomp_cvalue cv -> eval_cvalue env cv
    | Mccomp_clause cls -> unhandled_comp ~desc:"clses" ()

  and eval_cvalue env cv =
    match cv with
    | Mcvalue_ivalue iv -> eval_ivalue env iv
    | Mcvalue_ctr (k, vs) -> unhandled_comp ~desc:"ctr" ()
    | Mcvalue_thunk cc -> unhandled_comp ~desc:"suscomp" ()

  and eval_ivalue env iv =
    match iv with
    | Mivalue_var v -> if ENV.mem v env then eval_hdr env v
      else eval_var env v
    | Mivalue_sig s -> eval_sig env s
    | Mivalue_int n -> return (VInt n)
    | Mivalue_bool b -> return (VBool b)
    | Mivalue_icomp ic -> eval_icomp env ic

  and eval_icomp env ic =
    match ic with
    | Micomp_force iv -> eval_app env iv []
    | Micomp_app (iv, cs) -> eval_app env iv cs

  and eval_app env u cs =
    let mhdr cs = eval_ivalue env u >>=
      function VMultiHandler f -> f cs
      | _ as v -> not_hdr ~desc:(vshow v) () in
    mhdr (List.map (eval_ccomp env) cs)

  and eval_var env x =
    Debug.print "Searching for %s...\n" x; return (VInt 2)

  and eval_hdr env f = Debug.print "Looking for %s...\n" f; ENV.find f env

  and eval_sig env s =
    return (VMultiHandler
	      (fun cs -> sequence cs >>= fun vs -> command s vs))

end

