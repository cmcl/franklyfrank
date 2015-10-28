(***********************************************************************
 * Collection of helper functions to construct components of the
 * parse tree (untyped AST). Inspired by the frontend design of the
 * OCaml compiler.
 * 
 *
 * Created by Craig McLaughlin on 03/07/2015.
 ***********************************************************************
 *)

open ParseTree

module Term = struct
  let datatype dtd = Sterm_datatype dtd
  let value_decl vdecl = Sterm_vdecl vdecl
  let value_defn vdefn = Sterm_vdefn vdefn
  let effect_in effin = Sterm_effin effin
end

module Datatype = struct
  let mk name ?(params = []) ?(ctrs = []) () =
    { 
      sdt_name = name;
      sdt_parameters = params;
      sdt_constructors = ctrs
    }

  let constr_decl name ?(args = []) res =
    {
      sctr_name = name;
      sctr_args = args;
      sctr_res = res
    }
end

module CComputation = struct
  let cvalue cval = CComp_cvalue cval

  let clause pats comp = CComp_hdr_clause (pats, comp)

  let compose clauses = CComp_compose (clauses)
end

module CValue = struct
  let ivalue ival = CValue_ivalue ival

  let sus_comp scomp = CValue_thunk scomp

  let ctr name args = CValue_ctr (name, args)
end

module IValue = struct
  let ident name = IValue_ident name

  let integer n = IValue_int n

  let float f = IValue_float f

  let boolean b = IValue_bool b

  let str s = IValue_str s

  let icomp ic = IValue_icomp ic
end

module IComp = struct
  let app func ?(args = []) () = IComp_app (func, args)
end

module EffInterface = struct
  let mk name ?(params = []) ?(cmds = []) () =
    {
      sei_name = name;
      sei_parameters = params;
      sei_commands = cmds
    }

  let cmd_decl name ?(args = []) res =
    {
      scmd_name = name;
      scmd_args = args;
      scmd_res = res
    }
  end

module ValueDecl = struct
  let mk name stype = { svdecl_name = name; svdecl_type = stype }
end

module TypExp = struct
  let mk d = { styp_desc = d }

  let type_variable_counter = ref 0
  let fresh_tvar () = incr type_variable_counter; !type_variable_counter

  let tvar name = mk (Styp_tvar name)
  let fresh_rigid_tvar name =
    let n = fresh_tvar () in mk (Styp_rtvar (name, n))
  let fresh_flexi_tvar name =
    let n = fresh_tvar () in mk (Styp_ftvar (name, n))

  let datatype name ts = mk (Styp_datatype (name, ts))
  let sus_comp typ_exp = mk (Styp_thunk typ_exp)

  let comp ?(args = []) res = mk (Styp_comp (args,res))

  let returner v ?(effs = []) () = mk (Styp_ret (effs,v))

  let effin name ?(params = []) () = mk (Styp_effin (name, params))

  let bool () = mk (Styp_bool)
  let int () = mk (Styp_int)
  let float () = mk (Styp_float)
  let str () = mk (Styp_str)

  (* The one and only effect variable with a special non-parsable name to
     avoid conflicts. *)
  let effect_var_set = [fresh_rigid_tvar "£"]
  let closed_effect_set = [fresh_rigid_tvar "@"]
  let eff_set xs = mk (Styp_eff_set xs)
end

module ValueDefn = struct
  let mk name ?(pats = []) ccomp =
    {
      vdef_name = name;
      vdef_args = pats;
      vdef_comp = ccomp
    }
end

module Pattern = struct
  let mk d = { spat_desc = d }

  let vpat vp = mk (Spat_value vp)
  let cpat cp = mk (Spat_comp cp)
  let any () = mk Spat_any
  let thunk thk = mk (Spat_thunk thk)

  let any_value () = Svpat_any
  let var name = Svpat_var name
  let integer n = Svpat_int n
  let float f = Svpat_float f
  let boolean b = Svpat_bool b
  let str s = Svpat_str s
  let ctr name ?(pats = []) () = Svpat_ctr (name, pats)

  let request name ?(pats = []) cont = Scpat_request (name, pats, cont)

end
