module Parser
open Types
open ParserMonad
#nowarn "40"

(* Parse lines to their Terms *)
let rec parseLines =
    Parser(fun cs -> 
        match cs with
        | l -> 
            let code,rest = findUntilSkipCodeBlock l Token.Semicolon
            let term = parseLineToTerm code
            if rest.Length > 0 then
                let nextLines, rest2 = parse parseLines rest
                match nextLines with
                | Some(l) -> Some(term :: l), rest2
                | None -> Some([term]), rest2
            else
                Some([term]), rest)

and parseLineToTerm l =
    // Check the line against parsers that may apply
    let result, r = parse (parseVarDeclaration <|> parseVarAssign <|> parseIfElse <|> parseEOF <|> parseWhile <|> parsePrint) l
    // Just error if a line cannot be parsed
    match result with
    | Some(x) -> x
    | None -> failwithf "Cannot parse line to term (%A)" l

(* Parse some Tokens *)
and parseToken t = 
    Parser(fun cs -> 
        match cs with
        | x :: xs -> 
            if x = t then Some(()), xs
            else None, xs
        | [] -> None, [])

and parseVarId = 
    Parser(fun cs -> 
        match cs with
        | x :: xs -> 
            match x with
            | Token.VarId(x) -> Some(x), xs
            | _ -> None, xs
        | [] -> None, [])

(* Parse an arithmetic or conditional expression *)
and parseExpressionUntil (t:Token) =
    Parser(fun cs -> 
        match cs with
        | l -> 
            let cond,rest = findUntil l t
            if cond.Length > 0 then
                let c, r = parseBrackets cond
                Some(collapse [c]), Token.OpenBrace :: rest
            else
                None, rest)
(* Parse the defined Terms *)
and parseVarDeclaration =
    parser {
        do! parseToken Token.Var
        let! id = parseVarId
        do! parseToken Token.Equals
        let! expr = parseExpressionUntil Token.Semicolon
        return Term.VarDecl(id, expr)
    }

and parseVarAssign =
    parser {
        let! id = parseVarId
        do! parseToken Token.VarAssign
        let! expr = parseExpressionUntil Token.Semicolon
        return Term.VarAssign(id,expr)
    }

and parseIfElse =
    parser {
        do! parseToken Token.If
        let! cond = parseExpressionUntil Token.OpenBrace
        let! ifBlock = parseCodeBlock
        do! parseToken Token.Else
        let! elseBlock = parseCodeBlock
        return Term.If(cond, ifBlock, elseBlock)
    }

and parseWhile =
    parser {
        do! parseToken Token.While
        let! cond = parseExpressionUntil Token.OpenBrace
        let! block = parseCodeBlock
        return Term.While(cond, block)
    }

and parsePrint =
    parser {
        do! parseToken Token.Print
        let! expr = parseExpressionUntil Token.OpenBrace
        return Term.Print(expr)
    }

and parseCodeBlock =
    Parser(fun cs -> 
        match cs with
        | x :: xs -> 
            let code,rest = findUntilSkipCodeBlock xs Token.CloseBrace
            if code.Length > 0 then
                let block, rest2 = parse parseLines code
                match block with
                | Some(x :: xs) -> Some(Term.CodeBlock(x::xs)), rest
                | Some([]) -> None, rest
                | None -> None, rest
            else
                None, rest
        | [] -> None, [])

and parseEOF =
    parser {
        do! parseToken Token.EOF
        return Term.EOF
    }

let doParse (l:List<Token>) =
    parse parseLines l