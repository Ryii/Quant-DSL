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
  | ArrayLit of expr list
  | DictLit of (expr * expr) list
  | Index of expr * expr

and binop = Add | Sub | Mul | Div | Pow | Eq | Neq | Lt | Gt | Le | Ge | And | Or
and unop = Neg | Not

and typ =
  | TFloat
  | TString
  | TBool
  | TFunc of typ list * typ
  | TArray of typ
  | TDict of typ * typ

type value =
  | VFloat of float
  | VString of string
  | VBool of bool
  | VArray of value list
  | VDict of (value * value) list

let rec string_of_value = function
  | VFloat f -> string_of_float f
  | VString s -> "\"" ^ s ^ "\""
  | VBool b -> string_of_bool b
  | VArray values -> 
      "[" ^ (String.concat ", " (List.map string_of_value values)) ^ "]"
  | VDict entries -> 
      "{" ^ 
      (String.concat ", " 
         (List.map (fun (k, v) -> 
           string_of_value k ^ ": " ^ string_of_value v) entries)) ^ 
      "}"

let rec string_of_typ = function
  | TFloat -> "float"
  | TString -> "string"
  | TBool -> "bool"
  | TFunc(_, _) -> "function"
  | TArray t -> "array of " ^ string_of_typ t
  | TDict (kt, vt) -> "dict of " ^ string_of_typ kt ^ " to " ^ string_of_typ vt