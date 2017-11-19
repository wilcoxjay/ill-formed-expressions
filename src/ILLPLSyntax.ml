type var = string
type funname = string

type unop = Not | Neg
type binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Le | Gt | Ge

let unop_to_string = function
  | Not -> "!"
  | Neg -> "-"

let binop_to_string = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Eq  -> "=="
  | Neq -> "!="
  | Lt  -> "<"
  | Le  -> "<="
  | Gt  -> ">"
  | Ge  -> ">="

type expr =
  | Unit
  | Bool of bool
  | Var of var
  | Num of int
  | List of expr list
  | Let of var * expr * expr
  | Sequence of expr * expr
  | If of expr * expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Implies of expr * expr
  | Funcall of funname * expr list
  | Forall of var * expr
  | Exists of var * expr
  | Assert of expr
  | Length of expr
  | Index of expr * expr
  | Slice of expr * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr

type requires = Requires of expr
type ensures = Ensures of var * expr

type binding = Fun of funname * var list * requires * ensures * expr
