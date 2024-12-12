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

%left PLUS MINUS
%left TIMES DIVIDE
%right POW
%nonassoc UMINUS

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
  | expr LBRACKET expr RBRACKET { Index($1, $3) }
  | MINUS expr %prec UMINUS { UnaryOp(Neg, $2) }
  | LPAREN expr RPAREN { $2 }
  | FLOAT            { FloatLit($1) }
  | STRING           { StringLit($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | IDENT            { Var($1) }
  | IDENT LPAREN arg_list RPAREN { FuncCall($1, $3) }
  | LBRACKET array_elements RBRACKET { ArrayLit($2) }
  | LBRACE dict_elements RBRACE { DictLit($2) }
  | FUNCTION LPAREN param_list RPAREN LBRACE stmt_list RBRACE { Lambda($3, $6) }

arg_list:
  | /* empty */ { [] }
  | expr_list { $1 }

expr_list:
  | expr { [$1] }
  | expr_list COMMA expr { $1 @ [$3] }

array_elements:
  | /* empty */ { [] }
  | expr_list { $1 }

dict_elements:
  | /* empty */ { [] }
  | dict_pair_list { $1 }

dict_pair_list:
  | dict_pair { [$1] }
  | dict_pair_list COMMA dict_pair { $1 @ [$3] }

dict_pair:
  | expr COLON expr { ($1, $3) }

param_list:
  | /* empty */ { [] }
  | IDENT { [$1] }
  | param_list COMMA IDENT { $1 @ [$3] }