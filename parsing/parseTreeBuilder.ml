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
end

module EffInterface = struct
  let mk name ?(params = []) ?(sigs = []) () =
    {
      sei_name = name;
      sei_parameters = params;
      sei_signatures = sigs
    }
  end

module ValueDecl = struct
  let mk name stype = { svdecl_name = name; svdecl_type = stype }
end

module Type = struct
  let mk d = { styp_desc = d }

  let var name = mk (Styp_var name)
  let arrow a b = mk (Styp_arrow (a, b))
  let constr name typ_exp = mk (Styp_ctr (name, typ_exp))
  let effect_sig name typ_exp = mk (Styp_effin (name, typ_exp))
end

module ValueDefn = struct
  let mk name ?(args = []) ccomp =
    {
      vdef_name = name;
      vdef_args = args;
      vdef_comp = ccomp
    }
end
