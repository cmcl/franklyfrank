%{

  open Parsetree

%}

%token ARROW
%token BAR
%token COLON
%token COMMA
%token DATA
%token DEDENT
%token EOF
%token EQUAL
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE
%token FALSE
%token INDENT
%token LBRACKET
%token RBRACKET
%start program

%%

program:
  | EOF        { None }
  | p = decl   { Some p }
  ;

decl:
  | DATA ID opt_type_parameters EQUAL constructor_decls
      { (* make a datatype here *) }
  | ID COLON type_definition { (* make a handler here *) }
  ;

opt_type_parameters:
  | (* empty *)      { [] }
  | type_parameter_list { List.rev $1 }
  ;

type_parameter_list:
  | type_variable    { [$1] }
  | type_parameter_list type_variable { $2 :: $1 }
  ;

type_variable:
  | ID       { (* make a type variable here *) }
  ;

constructor_decls:
  | (* empty *)                            { [] }
  | constructor_decl                       { [$1] }
  | bar_constructor_decl                   { [$1] }
  | constructor_decls bar_constructor_decl { $2 :: $1 }
  ;
