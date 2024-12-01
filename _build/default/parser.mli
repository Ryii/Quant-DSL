
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TO
  | TIMES
  | THEN
  | STRING of (string)
  | SEMI
  | RPAREN
  | RETURN
  | RBRACKET
  | RBRACE
  | POW
  | PLUS
  | OR
  | NOT
  | NEWLINE
  | NEQ
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBRACKET
  | LBRACE
  | IN
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUNCTION
  | FOR
  | FLOAT of (float)
  | FALSE
  | EQUAL
  | EQ
  | EOF
  | END
  | ELSE
  | DO
  | DIVIDE
  | CONTINUE
  | COMMA
  | COLON
  | BREAK
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val program: Lexing.position -> (Ast.program) MenhirInterpreter.checkpoint
  
end
