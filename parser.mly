%{
open Ast
%}

%token <float> FLOAT
%token <string> IDENT
%token <string> STRING
%token LET IN IF THEN ELSE TRUE FALSE FUNCTION RETURN
%token WHILE DO FOR TO BREAK CONTINUE AND OR NOT
%token PLUS MINUS TIMES DIVIDE POW EQ EQUAL NEQ LT GT LE GE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMI COLON COMMA NEWLINE
%token EOF END

// %token <float> FLOAT
// %token <string> IDENT
// %token <string> STRING
// %token LET IF THEN ELSE RETURN
// %token WHILE DO IN
// %token PLUS MINUS TIMES DIVIDE POW EQ
// %token LPAREN RPAREN SEMI COMMA
// %token EOF END

%type <Ast.program> program
%type <Ast.expr> expr

%start program

%%

program:
  | stmt_list EOF { Program($1) }

stmt_list:
  | /* empty */ { [] }
  | stmt_list stmt { $1 @ [$2] }

stmt:
  | expr SEMI { ExprStmt($1) }
  | LET IDENT EQ expr SEMI { LetStmt($2, $4) }
  | RETURN expr SEMI { ReturnStmt($2) }
  | IF expr THEN stmt_list ELSE stmt_list END { IfStmt($2, $4, $6) }
  | WHILE expr DO stmt_list END { WhileStmt($2, $4) }

expr:
  | expr PLUS expr   { BinOp(Add, $1, $3) }
  | expr MINUS expr  { BinOp(Sub, $1, $3) }
  | expr TIMES expr  { BinOp(Mul, $1, $3) }
  | expr DIVIDE expr { BinOp(Div, $1, $3) }
  | expr POW expr    { BinOp(Pow, $1, $3) }
  | MINUS expr       { UnaryOp(Neg, $2) }
  | LPAREN expr RPAREN { $2 }
  | FLOAT            { FloatLit($1) }
  | STRING           { StringLit($1) }
  | IDENT            { Var($1) }
  | IDENT LPAREN arg_list RPAREN { FuncCall($1, $3) }

arg_list:
  | /* empty */ { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }
