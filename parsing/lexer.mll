(***********************************************************************
 * Lexer for Frank source language.
 *
 * Created by Craig McLaughlin on 1/6/2015.
 ***********************************************************************
 *)

{
  open Lexing
  open Parser
  open ErrorHandling

  let comment_depth = ref 0

}

let int = '-'? ['0'-'9']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let alpha =  ['a'-'z' 'A'-'Z' '_']
let uppercase = ['A'-'Z']
let alphanumeric = alpha | ['0'-'9']
let id = alpha (alphanumeric | ['\''])*

rule token = parse
  | white     { token lexbuf }
  | newline   { new_line lexbuf; token lexbuf }
  | int       { INTLIT (int_of_string (Lexing.lexeme lexbuf)) }
  | "!"         { BANG }
  | "data"      { DATA }
  | "interface" { INTERFACE }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | '{'         { LBRACE }
  | "{-"        { comment_depth := !comment_depth + 1; comment lexbuf }
  | '['       { LBRACKET }
  | '('       { LPAREN }
  | '}'       { RBRACE }
  | ']'       { RBRACKET }
  | ')'       { RPAREN }
  | ':'       { COLON }
  | '='       { EQUAL }
  | '|'       { BAR }
  | "->"      { RARROW }
  | '.'       { DOT }
  | ','       { COMMA }
  | '"'       { read_string (Buffer.create 20) lexbuf }
  | '_'       { UNDERSCORE }
  | uppercase alphanumeric* { UID (Lexing.lexeme lexbuf) }
  | id        { ID (Lexing.lexeme lexbuf) }
  | _         { raise (SyntaxError ("Unexpected character: " ^
				       Lexing.lexeme lexbuf)) }
  | eof       { EOF }

and comment = parse
  | "{-"      { comment_depth := !comment_depth + 1; comment lexbuf }
  | "-}"      { comment_depth := !comment_depth - 1;
		if !comment_depth == 0 then token lexbuf else comment lexbuf }
  | newline   { new_line lexbuf; comment lexbuf }
  | _ { comment lexbuf }
  | eof { raise (SyntaxError ("Comment not terminated.")) }

and read_string buf = parse
  | '"'               { STRLIT (Buffer.contents buf) }
  | '\\' '\\'         { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '\n'         { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' '\r'         { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' '\t'         { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+     { Buffer.add_string buf (Lexing.lexeme lexbuf);
			read_string buf lexbuf }
  | _                 { raise (SyntaxError ("Illegal string character " ^
					       Lexing.lexeme lexbuf)) }
  | eof               { raise (SyntaxError ("Non-terminating string")) }
