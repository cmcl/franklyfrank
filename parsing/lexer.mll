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

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
	         pos_lnum = pos.pos_lnum + 1
      }
}

let int = '-'? ['0'-'9']+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let alpha =  ['a'-'z' 'A'-'Z' '_']
let alphanumeric = alpha | ['0'-'9']
let id = alpha (alphanumeric | ['\''])*

rule token = parse
  | white     { token lexbuf }
  | newline   { next_line lexbuf; token lexbuf }
  | "!"         { BANG }
  | "data"      { DATA }
  | "interface" { INTERFACE }
  | '{'         { LBRACE }
  | '['       { LBRACKET }
  | '('       { LPAREN }
  | '}'       { RBRACE }
  | ']'       { RBRACKET }
  | ')'       { RPAREN }
  | ':'       { COLON }
  | ';'       { SEMI }
  | '='       { EQUAL }
  | '|'       { BAR }
  | "->"      { LARROW }
  | '.'       { DOT }
  | ','       { COMMA }
  | id        { ID (Lexing.lexeme lexbuf) }
  | _         { raise (SyntaxError ("Unexpected character: " ^
				       Lexing.lexeme lexbuf)) }
  | eof       { EOF}
