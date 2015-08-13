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

  let boolean b = IValue_bool b

  let icomp ic = IValue_icomp ic
end

module IComp = struct
  let app func ?(args = []) () = IComp_app (func, args)
end

module EffInterface = struct
  let mk name ?(params = []) ?(sigs = []) () =
    {
      sei_name = name;
      sei_parameters = params;
      sei_signatures = sigs
    }

  let sig_decl name ?(args = []) res =
    {
      ssig_name = name;
      ssig_args = args;
      ssig_res = res
    }
  end

module ValueDecl = struct
  let mk name stype = { svdecl_name = name; svdecl_type = stype }
end

module TypExp = struct
  let mk d = { styp_desc = d }

  let rigid_tvar name = mk (Styp_rtvar name)
  let flexi_tvar name = mk (Styp_ftvar name)
  let ctr name tes = mk (Styp_ctr (name, tes))
  let sus_comp typ_exp = mk (Styp_thunk typ_exp)

  let comp ?(args = []) res = mk (Styp_comp (args,res))

  let returner v ?(effs = []) () = mk (Styp_ret (effs,v))

  let poly ts t = mk (Styp_poly (ts, t))

  let effin name ?(params = []) () = mk (Styp_effin (name, params))

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

  let var name = Svpat_var name

  let integer n = Svpat_int n

  let boolean b = Svpat_bool b

  let ctr name ?(pats = []) () = Svpat_ctr (name, pats)

  let request name ?(pats = []) cont = Scpat_request (name, pats, cont)

  let thunk thk = Scpat_thunk thk
end
