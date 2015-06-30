module ParserOld

open Types
open ParserMonad

(* Parse a list of Tokens to an Expression (arithmetic, boolean expressions) *)
let rec parseBrackets ts = 
    match ts with
    | OpenBracket :: ts1 -> 
        let es1, ts2 = parseBrackets ts1
        match ts2 with
        | CloseBracket :: ts3 -> 
            let es2, ts3 = parseBrackets ts3
            Sequence([ es1; es2 ]), ts3
        | _ -> failwithf "unmatched open bracket %A" ts
    | CloseBracket :: _ -> Sequence [], ts
    | Token.EOF :: [] -> Sequence [], []
    | t :: ts1 -> 
        let e = 
            match t with
            | Token.Int x -> Const(Value.Int(x)) |> Collapsed
            | Token.Bool x -> Const(Value.Bool(x)) |> Collapsed
            | VarId x -> Var(x) |> Collapsed
            | _ -> Token t
        
        let es, ts2 = parseBrackets ts1
        merge e es, ts2
    | [] -> Sequence [], []

and merge (e1 : BracketizedExpr) (e2 : BracketizedExpr) = 
    let es1, es2 = 
        (match e1 with
         | Sequence es -> es
         | e -> [ e ]), 
        (match e2 with
         | Sequence es -> es
         | e -> [ e ])
    Sequence(es1 @ es2)

let rec collapse es = 
    match es with
    | Sequence e :: es1 -> collapse (Collapsed(collapse e) :: es1)
    | x :: Sequence y :: es1 -> collapse (Collapsed(collapse (x :: y)) :: es1)
    | _ -> 
        let es1 = collapseTimes es
        let es2 = collapsePlus es1
        let es3 = collapseGreaterThan es2
        match es3 with
        | [ Collapsed res ] -> res
        | _ -> failwithf "Cannot collapse %A" es

and collapseGreaterThan (l : List<BracketizedExpr>) : List<BracketizedExpr> = 
    match l with
    | Collapsed(x) :: Token(Token.Plus) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.Plus) :: (Collapsed(y) :: xs |> collapseGreaterThan)
    | Collapsed(x) :: Token(Token.Times) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.Times) :: (Collapsed(y) :: xs |> collapseGreaterThan)
    | Collapsed(x) :: Token(Token.GreaterThan) :: Collapsed(y) :: xs -> 
        (Collapsed(Expr.GreaterThan(x, y)) :: xs) |> collapseGreaterThan
    | Collapsed(x) :: Collapsed(y) :: xs -> Collapsed(x) :: (Collapsed(y) :: xs |> collapseGreaterThan)
    | Collapsed(e) :: [] -> Collapsed e :: []
    | _ -> failwithf "Cannot collapse %A" l

and collapsePlus (l : List<BracketizedExpr>) : List<BracketizedExpr> = 
    match l with
    | Collapsed(x) :: Token(Token.Times) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.Times) :: (Collapsed(y) :: xs |> collapsePlus)
    | Collapsed(x) :: Token(Token.GreaterThan) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.GreaterThan) :: (Collapsed(y) :: xs |> collapsePlus)
    | Collapsed(x) :: Token(Token.Plus) :: Collapsed(y) :: xs -> (Collapsed(Expr.Plus(x, y)) :: xs) |> collapsePlus
    | Collapsed(x) :: Collapsed(y) :: xs -> Collapsed(x) :: (Collapsed(y) :: xs |> collapsePlus)
    | Collapsed(e) :: [] -> Collapsed e :: []
    | _ -> failwithf "Cannot collapse %A" l

and collapseTimes (l : List<BracketizedExpr>) : List<BracketizedExpr> = 
    match l with
    | Collapsed(x) :: Token(Token.Plus) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.Plus) :: (Collapsed(y) :: xs |> collapseTimes)
    | Collapsed(x) :: Token(Token.GreaterThan) :: Collapsed(y) :: xs -> 
        Collapsed(x) :: Token(Token.GreaterThan) :: (Collapsed(y) :: xs |> collapseTimes)
    | Collapsed(x) :: Token(Token.Times) :: Collapsed(y) :: xs -> (Collapsed(Expr.Times(x, y)) :: xs) |> collapseTimes
    | Collapsed(x) :: Collapsed(y) :: xs -> Collapsed(x) :: (Collapsed(y) :: xs |> collapseTimes)
    | Collapsed(e) :: [] -> Collapsed e :: []
    | _ -> failwithf "Cannot collapse %A" l

(* Helper function to find the Tokens until a specified Token is found *)
let rec findUntil (l : List<Token>) (t : Token) : List<Token> * List<Token> = 
    let rec result (l : List<Token>) = 
        match l with
        | x :: xs when x = t -> []
        | x :: [] -> [ x ]
        | x :: xs -> x :: (result xs)
        | [] -> failwithf "Cannot find Token: %A" t
    
    let rec getRest (line : List<Token>) (full : List<Token>) = 
        match line with
        | x :: xs when x = full.Head -> getRest line.Tail full.Tail
        | [] when full.Length > 0 && full.Head = t -> full.Tail
        | _ -> full
    
    let line = result l
    line, getRest line l

(* Skip parsing in code blocks, this will be done later *)
let rec findUntilSkipCodeBlock (l : List<Token>) (t : Token) : List<Token> * List<Token> = 
    let rec result (l : List<Token>) = 
        match l with
        | x :: xs when x = Token.OpenBrace -> 
            let l, rest = findUntilSkipCodeBlock xs Token.CloseBrace
            x :: l @ [ Token.CloseBrace ] @ result rest
        | x :: xs when x = t -> []
        | x :: [] -> [ x ]
        | x :: xs -> x :: (result xs)
        | _ -> failwithf "Cannot find Token: %A" t
    
    let rec getRest (line : List<Token>) (full : List<Token>) = 
        match line with
        | x :: xs when x = full.Head -> getRest line.Tail full.Tail
        | [] when full.Length > 0 && full.Head = t -> full.Tail
        | _ -> full
    
    let line = result l
    line, getRest line l

(* Split the program up in lines, seperated by the Semicolon Token *)
let rec parseLines (l : List<Token>) : List<Term> = 
    match l with
    | x :: xs -> 
        let line, rest = findUntilSkipCodeBlock l Token.Semicolon
        (parseLineToTerm line) :: (parseLines rest)
    | [] -> []

(* Parse a line to a valid Statement (Term) *)
and parseLineToTerm (l : List<Token>) : Term = 
    match l with
    | Token.Var :: Token.VarId x :: Token.Equals :: xs -> 
        let expr, _ = parseBrackets xs
        Term.VarDecl(x, collapse [ expr ])
    | Token.VarId x :: Token.VarAssign :: xs -> 
        let expr, _ = parseBrackets xs
        Term.VarAssign(x, collapse [ expr ])
    | Token.If :: xs -> 
        let cond, rest1 = parseConditionalExpr xs
        let ifblock, rest2 = parseCodeBlock rest1
        
        let elseblock, rest3 = 
            match rest2 with
            | Token.Else :: Token.OpenBrace :: xs -> 
                let block, rest = findUntilSkipCodeBlock xs Token.CloseBrace
                
                let lines = 
                    [ for x in block -> x ]
                    |> parseLines
                CodeBlock(lines), rest
            | _ -> failwithf "Not a valid conditional statement (else not found)"
        Term.If(collapse [ cond ], ifblock, elseblock)
    | Token.While :: xs -> 
        let cond, rest1 = parseConditionalExpr xs
        let doblock, rest2 = parseCodeBlock rest1
        Term.While(collapse [ cond ], doblock)
    | Token.Print :: xs -> // I made this for debugging purposes
        let cond, rest1 = parseConditionalExpr xs
        Term.Print(collapse [ cond ])
    | Token.EOF :: [] -> Term.EOF
    | _ -> failwithf "Cannot parse line %A to term" l

(* Parse the conditional expression *)
and parseConditionalExpr l = 
    let tokens, rest = findUntil l Token.OpenBrace
    let expr, _ = parseBrackets tokens
    expr, Token.OpenBrace :: rest

(* Parse a block of code enclosed by { and } *)
and parseCodeBlock (l : List<Token>) = 
    match l with
    | Token.OpenBrace :: xs -> 
        let block, rest = findUntilSkipCodeBlock xs Token.CloseBrace
        
        let lines = 
            [ for x in block -> x ]
            |> parseLines
        CodeBlock(lines), rest
    | _ -> failwithf "Not a valid conditional statement"
