module Types

(* Valid and parsable Tokens for the language *)
type Token = 
    | Var
    | VarAssign
    | If
    | Do
    | Else
    | End
    | While
    | Print
    | Semicolon
    | Colon
    | Equals
    | Plus
    | Minus
    | Times
    | Division
    | GreaterThan
    | SmallerThan
    | OpenBracket
    | CloseBracket
    | OpenBrace
    | CloseBrace
    | IntType
    | BoolType
    | Int of int
    | Bool of bool
    | VarId of string
    | Type of string
    | EOF

(* Defined statements in the language *)
type Term = 
    | VarDecl of string * Expr
    | VarAssign of string * Expr
    | CodeBlock of List<Term>
    | If of Expr * Term * Term
    | While of Expr * Term
    | Print of Expr
    | EOF

(* Defined types of Expressions in the language *)
and BracketizedExpr = 
    | Sequence of List<BracketizedExpr>
    | Token of Token
    | Collapsed of Expr

and Expr = 
    | Plus of Expr * Expr
    | Times of Expr * Expr
    | GreaterThan of Expr * Expr
    | Const of Value
    | Var of string

and Value = 
    | Int of int
    | Bool of bool

type Type = 
    | Int
    | Bool
