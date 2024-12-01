(* ast.ml *)
type program = Program of stmt list
and stmt =
  | LetStmt of string * expr
  | ExprStmt of expr
  | ReturnStmt of expr
  | IfStmt of expr * stmt list * stmt list
  | WhileStmt of expr * stmt list
and expr =
  | FloatLit of float
  | StringLit of string
  | BoolLit of bool
  | Var of string
  | BinOp of binop * expr * expr
  | UnaryOp of unop * expr
  | FuncCall of string * expr list
  | Lambda of string list * stmt list
and binop = Add | Sub | Mul | Div | Pow | Eq | Neq | Lt | Gt | Le | Ge | And | Or
and unop = Neg | Not
