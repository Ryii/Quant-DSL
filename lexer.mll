{
(* lexer.mll *)
open Parser

let keyword_table = Hashtbl.create 32

let _ =
  List.iter (fun (k, v) -> Hashtbl.add keyword_table k v)
    [
      ("let", LET);
      ("in", IN);
      ("if", IF);
      ("then", THEN);
      ("else", ELSE);
      ("true", TRUE);
      ("false", FALSE);
      ("function", FUNCTION);
      ("return", RETURN);
      ("while", WHILE);
      ("do", DO);
      ("for", FOR);
      ("to", TO);
      ("break", BREAK);
      ("continue", CONTINUE);
      ("and", AND);
      ("or", OR);
      ("not", NOT);
    ]

let buffer = Buffer.create 16
}

rule read = parse
  | [' ' '\t' '\r'] { read lexbuf }  (* Skip whitespace *)
  | '\n'            { NEWLINE }
  | "/*"            { comment lexbuf }
  | "//"            { single_line_comment lexbuf }
  | ['0'-'9']+ ('.' ['0'-'9']+)? as num {
      FLOAT (float_of_string num)
    }
  | ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
      try Hashtbl.find keyword_table id with Not_found -> IDENT id
    }
  | '"'             { Buffer.clear buffer; string_literal lexbuf; STRING (Buffer.contents buffer) }
  | '+'             { PLUS }
  | '-'             { MINUS }
  | '*'             { TIMES }
  | '/'             { DIVIDE }
  | '^'             { POW }
  | '='             { EQ }
  | "=="            { EQUAL }
  | "!="            { NEQ }
  | '<'             { LT }
  | '>'             { GT }
  | "<="            { LE }
  | ">="            { GE }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | '['             { LBRACKET }
  | ']'             { RBRACKET }
  | ';'             { SEMI }
  | ':'             { COLON }
  | ','             { COMMA }
  | eof             { EOF }
  | _ as c          { failwith (Printf.sprintf "Unexpected character: %c" c) }

and string_literal = parse
  | '"'             { () }  (* End of string *)
  (* | '\\' (
      'n'           { Buffer.add_char buffer '\n'; string_literal lexbuf }
    | 't'           { Buffer.add_char buffer '\t'; string_literal lexbuf }
    | '\\'          { Buffer.add_char buffer '\\'; string_literal lexbuf }
    | '"'           { Buffer.add_char buffer '"'; string_literal lexbuf }
    | _             { failwith "Invalid escape sequence" }
    ) *)
  | '\n'            { failwith "Unterminated string" }
  | eof             { failwith "Unterminated string" }
  | _ as c          { Buffer.add_char buffer c; string_literal lexbuf }

and comment = parse
  | "*/"            { read lexbuf }
  | eof             { failwith "Unterminated comment" }
  | _               { comment lexbuf }

and single_line_comment = parse
  | '\n'            { read lexbuf }
  | eof             { EOF }
  | _               { single_line_comment lexbuf }
