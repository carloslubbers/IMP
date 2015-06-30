module Lexer
open LexerMonad
open Types

(* 
Some lexing monads to easily parse a char list to its corresponding Token(s)
parseToken just parses a word or symbol to a Token
parseIntToken and parseVarId will return a Token containing the correct value

The definition of the Lexer Monad is found in LexerMonad.fs
*)
let parseToken s (t : Token) = 
    lexer { 
        let! f = parseWord s
        if f.IsEmpty = false then return t
    }

let parseIntToken = 
    lexer { 
        let! i = parseMany' (parseDigit)
        let is = new System.String(i |> Seq.toArray)
        let parsed, result = System.Int32.TryParse(is)
        if parsed then return Token.Int(result)
    }

let parseVarId = 
    lexer { 
        let! alpha = parseMany' (parseLetter <|> parseDigit)
        return VarId(new System.String(alpha |> List.toArray))
    }

(* Apply the lexers onto the char list, returns the list of Tokens or fails when no valid Token was found *)
let rec tokenize (l : List<char>) : List<Token> = tokenize' l 0

and tokenize' (l : List<char>) (pos : int) : List<Token> = 
    let ws, nows = parse (parseMany (removeWhitespace)) l
    match nows with
    | [] -> [ Token.EOF ]
    | prg -> 
        let t, r = 
            (parse 
                 ((parseToken (tcl "var") Token.Var) <|> (parseToken (tcl "<-") Token.VarAssign) 
                  <|> (parseToken (tcl "if") Token.If) <|> (parseToken (tcl "do") Do) <|> (parseToken (tcl "else") Else) 
                  <|> (parseToken (tcl "end") End) <|> (parseToken (tcl "while") Token.While) 
                  <|> (parseToken (tcl "print") Token.Print) <|> (parseToken (tcl ";") Semicolon) 
                  <|> (parseToken (tcl ":") Colon) <|> (parseToken (tcl "=") Equals) 
                  <|> (parseToken (tcl "+") Token.Plus) <|> (parseToken (tcl "-") Minus) 
                  <|> (parseToken (tcl "*") Token.Times) <|> (parseToken (tcl "/") Division) 
                  <|> (parseToken (tcl ">") Token.GreaterThan) <|> (parseToken (tcl "<") SmallerThan) 
                  <|> (parseToken (tcl "(") OpenBracket) <|> (parseToken (tcl ")") CloseBracket) 
                  <|> (parseToken (tcl "{") OpenBrace) <|> (parseToken (tcl "}") CloseBrace) 
                  <|> (parseToken (tcl "int") IntType) <|> (parseToken (tcl "bool") BoolType) 
                  <|> (parseToken (tcl "true") (Token.Bool(true))) <|> (parseToken (tcl "false") (Token.Bool(false))) 
                  <|> (parseIntToken) <|> (parseVarId)) prg)
        if t.IsSome && r.Length > 0 then t.Value :: (tokenize' r (pos + 1))
        elif t.IsSome && r.Length = 0 then [ t.Value ]
        else 
            do printfn "Syntax error after %A token(s)" pos
            []