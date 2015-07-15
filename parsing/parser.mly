%{

  open ParseTree
  open ParseTreeBuilder

  let mktyp k = Type.mk k

%}

%token BANG
%token BAR
%token COLON
%token DATA
%token DOT
%token EOF
%token EQUAL
%token <string> ID
%token INTERFACE
%token LARROW
%token LBRACE LBRACKET LPAREN
%token RBRACE RBRACKET RPAREN
%token NEWLINE

%left LARROW

%start <ParseTree.prog> program

%%

program:
  | list(term) EOF      { $1 }
  ;

term:
  | DATA ID opt_type_parameters EQUAL opt_constructor_decls DOT
      { Datatype.mk $2 ~params:$3 ~ctrs:$5 () |> Term.datatype }
  | ID COLON type_expression { ValueDecl.mk $1 $3 |> Term.value_decl }
  | INTERFACE ID opt_type_parameters EQUAL effect_signatures DOT
      { EffInterface.mk $2 ~params:$3 ~sigs:$5 () |> Term.effect_in }
  | ID pattern* EQUAL checkable_computation NEWLINE
      { ValueDefn.mk $1 ~pats:$2 $4 |> Term.value_defn }
  ;

checkable_computation:
  | checkable_value                     { CComputation.cvalue $1 }
  ;

paren_checkable_computation:
  | paren_checkable_value               { CComputation.cvalue $1 }
  ;

checkable_value:
  | inferable_value                        { CValue.ivalue $1 }
  | value_constructor                      { $1 }
  | suspended_computation                  { CValue.sus_comp $1 }
  ;

paren_checkable_value:
  | paren_inferable_value        { CValue.ivalue $1 }
  | LPAREN value_constructor RPAREN   { $2 }
  | suspended_computation             { CValue.sus_comp $1 }
  ;

inferable_value:
  | ID                            { IValue.ident $1 }
  | inferable_computation         { IValue.icomp $1 }
  ;

paren_inferable_value:
  | ID                            { IValue.ident $1 }
  | LPAREN inferable_computation RPAREN { IValue.icomp $2 }
  ;

inferable_computation:
  | inferable_value BANG        { IComp.forced_thunk $1 }
  | inferable_computation paren_checkable_computation { IComp.app $1 $2 }
  ;

value_constructor:
  | ID paren_checkable_value+           { CValue.ctr $1 $2 }
  ;

suspended_computation:
  | LBRACE checkable_computation RBRACE  { $2 }
  ;

pattern:
  | LPAREN pattern RPAREN         { $2 }
  | value_pattern                 { Pattern.vpat $1 }
  | LPAREN comp_pattern RPAREN    { Pattern.cpat $2 }
  ;

value_pattern:
  | ID                                    { Pattern.var $1 }
  | LPAREN ID value_pattern+ RPAREN       { Pattern.ctr $2 ~pats:$3 () }
  ;

comp_pattern:
  | ID value_pattern* LARROW ID    { Pattern.request $1 ~pats:$2 $4 }
  | ID BANG                        { Pattern.thunk $1 }

opt_type_parameters:
  | ps = list(type_variable)      { ps }
  ;

type_variable:
  | ID       { Type.var $1 }
  ;

effect_signatures:
  | effect_signature                       { [$1] }
  | bar_effect_signature                   { [$1] }
  | effect_signatures bar_effect_signature { $2 :: $1 }
  ;

effect_signature:
  | ID COLON type_expression               { Type.effect_sig $1 $3 }
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
  | ID COLON type_expression          { Type.constr $1 $3 }
  ;

bar_constructor_decl:
  | BAR constructor_decl                   { $2 }
  ;

type_expression:
  | type_variable                          { $1 }
  | LPAREN type_expression RPAREN          { $2 }
  | type_expression LARROW type_expression { Type.arrow $1 $3 }
  ;
