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
%token LET
%token IN
%token <string> ID
%token <string> STRLIT
%token INTERFACE
%token <int> INTLIT
%token <float> FLOATLIT
%token RARROW
%token LBRACE LBRACKET LPAREN
%token RBRACE RBRACKET RPAREN
%token TRUE
%token <string> UID
%token UNDERSCORE

%start <ParseTree.prog> program

%%

program:
  | list(term) EOF      { $1 }
  ;

term:
  | DATA UID opt_type_parameters EQUAL opt_constructor_decls DOT
      { Datatype.mk $2 ~params:$3 ~ctrs:$5 () |> Term.datatype }
  | ID COLON top_level_value_type DOT
      { ValueDecl.mk $1 $3 |> Term.value_decl }
  | INTERFACE ident opt_type_parameters EQUAL effect_commands DOT
      { EffInterface.mk $2 ~params:$3 ~cmds:$5 () |> Term.effect_in }
  | ID pattern* EQUAL pat_checkable_computation DOT
      { ValueDefn.mk $1 ~pats:$2 $4 |> Term.value_defn }
  ;

ident:
  | ID       { $1 }
  | UID      { $1 }
  ;

checkable_computation:
  | checkable_value                    { CComputation.cvalue $1 }
  | clauses = separated_list(BAR, pat_match_computation)
      { CComputation.compose clauses }
  ;

pat_checkable_computation:
  | checkable_value                { CComputation.cvalue $1 }
  (* | LPAREN *)
  (*     clauses = separated_list(BAR, pat_match_computation) *)
  (*   RPAREN *)
  (*     { CComputation.compose clauses } *)
  ;

pat_match_computation:
  | separated_nonempty_list(COMMA, pattern) RARROW
      pat_checkable_computation { CComputation.clause $1 $3 }
  ;

(* We'd like like to do this if we could get rid of the reduce/reduce
     conflicts. *)
(* pat_match_computation: *)
(*   | nonempty_list(pattern) RARROW *)
(*       pat_checkable_computation { CComputation.clause $1 $3 } *)
(*   ; *)

paren_checkable_computation:
  | paren_checkable_value               { CComputation.cvalue $1 }
  (* | LPAREN *)
  (*     clauses = separated_list(BAR, pat_match_computation) *)
  (*   RPAREN *)
  (*     { CComputation.compose clauses } *)
  ;

checkable_value:
  | inferable_value                        { CValue.ivalue $1 }
  | value_constructor                      { $1 }
  | suspended_computation                  { CValue.sus_comp $1 }
  ;

paren_checkable_value:
  | paren_inferable_value        { CValue.ivalue $1 }
  | paren_value_constructor      { $1 }
  | suspended_computation        { CValue.sus_comp $1 }
  ;

inferable_value:
  | ID                            { IValue.ident $1 }
  | INTLIT                        { IValue.integer $1 }
  | FLOATLIT                      { IValue.float $1 }
  | STRLIT                        { IValue.str $1 }
  | TRUE                          { IValue.boolean true }
  | FALSE                         { IValue.boolean false }
  | inferable_computation         { IValue.icomp $1 }
  ;

paren_inferable_value:
  | ID                            { IValue.ident $1 }
  | INTLIT                        { IValue.integer $1 }
  | FLOATLIT                      { IValue.float $1 }
  | STRLIT                        { IValue.str $1 }
  | TRUE                          { IValue.boolean true }
  | FALSE                         { IValue.boolean false }
  | LPAREN inferable_computation RPAREN { IValue.icomp $2 }
  ;

inferable_computation:
  | application                   { $1 }
  | let_binding                   { $1 }
;

application:
  | paren_inferable_value BANG list(paren_checkable_computation)
      { IComp.app $1 ~args:$3 () }
  | paren_inferable_value nonempty_list(paren_checkable_computation)
      { IComp.app $1 ~args:$2 () }
  ;

let_binding:
  | LET ID EQUAL pat_checkable_computation IN pat_checkable_computation
      { IComp.let_binding $2 $4 $6 }
;

value_constructor:
  | UID paren_checkable_value*           { CValue.ctr $1 $2 }
  ;

paren_value_constructor:
  | UID                             { CValue.ctr $1 [] }
  | LPAREN value_constructor RPAREN { $2 }

suspended_computation:
  | LBRACE checkable_computation RBRACE  { $2 }
  ;

pattern:
  | LPAREN pattern RPAREN             { $2 }
  | value_pattern                     { Pattern.vpat $1 }
  | LBRACKET comp_pattern RBRACKET    { Pattern.cpat $2 }
  | LBRACKET UNDERSCORE RBRACKET      { Pattern.any () }
  | LBRACKET ID RBRACKET              { Pattern.thunk $2 }
  ;

value_pattern:
  | ID                                    { Pattern.var $1 }
  | UID                                   { Pattern.ctr $1 () }
  | INTLIT                                { Pattern.integer $1 }
  | FLOATLIT                              { Pattern.float $1 }
  | STRLIT                                { Pattern.str $1 }
  | TRUE                                  { Pattern.boolean true }
  | FALSE                                 { Pattern.boolean false }
  | LPAREN UID value_pattern+ RPAREN      { Pattern.ctr $2 ~pats:$3 () }
  | UNDERSCORE                            { Pattern.any_value () }
  ;

comp_pattern:
  | ID value_pattern* RARROW ID    { Pattern.request $1 ~pats:$2 $4 }
  ;

opt_type_parameters:
  | ps = value_types      { ps }
  ;

type_variable:
  | ID       { TypExp.tvar $1 }
  ;

effect_commands:
  | effect_command                       { [$1] }
  | bar_effect_command                   { [$1] }
  | effect_commands bar_effect_command { $2 :: $1 }
  ;

effect_command:
  | ID COLON rargs = cmd_args
      { match rargs with
	| []
	  -> raise (SyntaxError ("Expecting command type"))
	     (* Will never happen! *)
	| res :: sgra -> EffInterface.cmd_decl $1 ~args:(List.rev sgra) res }

  ;

cmd_args:
  | cmd_arg                                { [$1] }
  | cmd_args RARROW cmd_arg                { $3 :: $1 }
  ;

cmd_arg:
  | value_type                             { $1 }
  ;

bar_effect_command:
  | BAR effect_command                   { $2 }
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
  | UID COLON rargs = constructor_args
      { match rargs with
	| []
	  -> raise (SyntaxError ("Expecting constructor type"))
	     (* Will never happen! *)
	| res :: sgra -> Datatype.constr_decl $1 ~args:(List.rev sgra) res }
  ;

constructor_args:
  | constructor_arg                         { [$1] }
  | constructor_args RARROW constructor_arg { $3 :: $1 }

constructor_arg:
  | LPAREN constructor_arg RPAREN          { $2 }
  | type_variable                          { $1 }
  | datatype                               { $1 }
  ;

bar_constructor_decl:
  | BAR constructor_decl                   { $2 }
  ;

top_level_value_type:
  | computation_type                       { TypExp.sus_comp $1 }
  ;

value_type:
  | LPAREN value_type RPAREN               { $2 }
  | type_variable                          { $1 }
  | datatype                               { $1 }
  | LBRACE computation_type RBRACE        { TypExp.sus_comp $2 }
  ;

datatype:
  | UID value_types               { TypExp.datatype $1 $2 }
  ;

paren_value_type:
  | LPAREN paren_value_type RPAREN         { $2 }
  | type_variable                          { $1 }
  | paren_datatype                         { $1 }
  | LBRACE computation_type RBRACE         { TypExp.sus_comp $2 }
  ;

paren_datatype:
  | UID                                        { TypExp.datatype $1 [] }
  | LPAREN UID non_empty_value_types RPAREN    { TypExp.datatype $2 $3 }
  ;

non_empty_value_types:
  | paren_value_type+                      { $1 }

value_types:
  | list(paren_value_type)                 { $1 }
  ;

computation_type:
  | arg                                    { TypExp.comp $1 }
  | rargs = arrow_type
        { match rargs with
	  | [] -> raise (SyntaxError ("Expecting function type"))
                  (* Impossible *)
	  | res :: sgra -> TypExp.comp ~args:(List.rev sgra) res
	}
  ;

arrow_type:
  | arg RARROW arg                         { [$3 ; $1] }
  | arrow_type RARROW arg                  { $3 :: $1  }
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
  | ident opt_type_parameters      { TypExp.effin $1 ~params:$2 () }
  ;
