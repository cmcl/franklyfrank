%{

  open ParseTree
  open ParseTreeBuilder
  open ErrorHandling

%}

%token BANG
%token BAR
%token COLON
%token COMMA
%token DATA
%token DOT
%token EOF
%token EQUAL
%token FALSE
%token <string> ID
%token INTERFACE
%token <int> INTLIT
%token LARROW
%token LBRACE LBRACKET LPAREN
%token RBRACE RBRACKET RPAREN
%token SEMI
%token TRUE

%left BAR
%left LARROW

%start <ParseTree.prog> program

%%

program:
  | list(term) EOF      { $1 }
  ;

term:
  | DATA ID opt_type_parameters EQUAL opt_constructor_decls DOT
      { Datatype.mk $2 ~params:$3 ~ctrs:$5 () |> Term.datatype }
  | ID COLON top_level_value_type { ValueDecl.mk $1 $3 |> Term.value_decl }
  | INTERFACE ID opt_type_parameters EQUAL effect_signatures DOT
      { EffInterface.mk $2 ~params:$3 ~sigs:$5 () |> Term.effect_in }
  | ID pattern* EQUAL checkable_computation SEMI
      { ValueDefn.mk $1 ~pats:$2 $4 |> Term.value_defn }
  ;

checkable_computation:
  | checkable_value                     { CComputation.cvalue $1 }
  | clauses = separated_nonempty_list(BAR, pat_match_computation)
      { CComputation.compose clauses }
  ;

pat_checkable_computation:
  | checkable_value                { CComputation.cvalue $1 }
  | LPAREN
      clauses = separated_nonempty_list(BAR, pat_match_computation)
    RPAREN
      { CComputation.compose clauses }
  ;

pat_match_computation:
  | separated_nonempty_list(COMMA, pattern) LARROW
      pat_checkable_computation { CComputation.clause $1 $3 }
  ;

paren_checkable_computation:
  | paren_checkable_value               { CComputation.cvalue $1 }
  | LPAREN
      clauses = separated_nonempty_list(BAR, pat_match_computation)
    RPAREN
      { CComputation.compose clauses }
  ;

checkable_value:
  | inferable_value                        { CValue.ivalue $1 }
  | value_constructor                      { $1 }
  | suspended_computation                  { CValue.sus_comp $1 }
  ;

paren_checkable_value:
  | paren_inferable_value        { CValue.ivalue $1 }
  | LPAREN value_constructor RPAREN   { $2 }
  | LPAREN suspended_computation RPAREN  { CValue.sus_comp $2 }
  ;

inferable_value:
  | ID                            { IValue.ident $1 }
  | INTLIT                        { IValue.integer $1 }
  | TRUE                          { IValue.boolean true }
  | FALSE                         { IValue.boolean false }
  | inferable_computation         { IValue.icomp $1 }
  ;

paren_inferable_value:
  | ID                            { IValue.ident $1 }
  | INTLIT                        { IValue.integer $1 }
  | TRUE                          { IValue.boolean true }
  | FALSE                         { IValue.boolean false }
  | LPAREN inferable_computation RPAREN { IValue.icomp $2 }
  ;

inferable_computation:
  | inferable_value BANG        { IComp.forced_thunk $1 }
  | inferable_value BANG nonempty_list(paren_checkable_computation)
      { IComp.app $1 $3 }
  ;

value_constructor:
  | ID paren_checkable_value+           { CValue.ctr $1 $2 }
  ;

suspended_computation:
  | LBRACE checkable_computation RBRACE  { $2 }
  ;

pattern:
  | LPAREN pattern RPAREN             { $2 }
  | value_pattern                     { Pattern.vpat $1 }
  | LBRACKET comp_pattern RBRACKET    { Pattern.cpat $2 }
  ;

value_pattern:
  | ID                                    { Pattern.var $1 }
  | INTLIT                                { Pattern.integer $1 }
  | TRUE                                  { Pattern.boolean true }
  | FALSE                                 { Pattern.boolean false}
  | LPAREN ID value_pattern+ RPAREN       { Pattern.ctr $2 ~pats:$3 () }
  ;

comp_pattern:
  | ID value_pattern* LARROW ID    { Pattern.request $1 ~pats:$2 $4 }
  | ID BANG                        { Pattern.thunk $1 }

opt_type_parameters:
  | ps = list(value_type)      { ps }
  ;

type_variable:
  | ID       { TypExp.var $1 }
  ;

effect_signatures:
  | effect_signature                       { [$1] }
  | bar_effect_signature                   { [$1] }
  | effect_signatures bar_effect_signature { $2 :: $1 }
  ;

effect_signature:
  | ID COLON rargs = sig_args
      { match rargs with
	| []
	  -> raise (SyntaxError ("Expecting signature type"))
	     (* Will never happen! *)
	| res :: sgra -> EffInterface.sig_decl $1 ~args:(List.rev sgra) res }

  ;

sig_args:
  | sig_arg                                { [$1] }
  | sig_args LARROW sig_arg                { $3 :: $1 }
  ;

sig_arg:
  | type_variable                          { $1 }
  | datatype                               { $1 }
  ;

bar_effect_signature:
  | BAR effect_signature                   { $2 }
  ;

opt_constructor_decls:
  | (* empty *)                            { [] }
  | constructor_decls                      { $1 }

constructor_decls:
  | constructor_decl                       { [$1] }
  | bar_constructor_decl                   { [$1] }
  | constructor_decls bar_constructor_decl { $2 :: $1 }
  ;

constructor_decl:
  | ID COLON rargs = constructor_args
      { match rargs with
	| []
	  -> raise (SyntaxError ("Expecting constructor type"))
	     (* Will never happen! *)
	| res :: sgra -> Datatype.constr_decl $1 ~args:(List.rev sgra) res }
  ;

constructor_args:
  | constructor_arg                         { [$1] }
  | constructor_args LARROW constructor_arg { $3 :: $1 }

constructor_arg:
  | type_variable                          { $1 }
  | datatype                               { $1 }
  ;

bar_constructor_decl:
  | BAR constructor_decl                   { $2 }
  ;

top_level_value_type:
  | value_type                             { $1 }
  | computation_types                      { TypExp.sus_comp $1 }
  ;

value_type:
  | type_variable                          { $1 }
  | datatype                               { $1 }
  | LBRACE computation_types RBRACE        { TypExp.sus_comp $2 }
  ;

datatype:
  | LPAREN ID value_type+ RPAREN           { TypExp.ctr $2 $3 }

computation_types:
  | returner                               { $1 }
  | rargs = arrow_type
        { match rargs with
	  | [] -> raise (SyntaxError ("Expecting function type"))
                  (* Impossible *)
	  | res :: sgra -> TypExp.comp (List.rev sgra) res
	}
  ;

arrow_type:
  | arg LARROW arg                         { [$1 ; $3] }
  | arrow_type LARROW arg                  { $3 :: $1  }
  ;

arg:
  | value_type                             { TypExp.returner $1 () }
  | returner                               { $1 }
  ;

returner:
  | LBRACKET effects RBRACKET value_type   { TypExp.returner $4 ~effs:$2 () }
  ;

effects:
  | es = separated_list(COMMA, effect_interface)  { es }
  ;

effect_interface:
  | ID opt_type_parameters      { TypExp.effin $1 ~params:$2 () }
  ;
