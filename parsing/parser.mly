%{

  open ParseTree
  open ParseTreeBuilder

  let mktyp k = Type.mk k

%}

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
  ;

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
  | type_expression LARROW type_expression { Type.arrow $1 $3 }
  ;
