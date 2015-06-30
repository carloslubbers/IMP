module ParserMonad
open Types

(* Build the Parser Monad *)
type Parser<'a> = 
    | Parser of (Token list -> (Option<'a> * Token list))

let parse (Parser p) = p

type ParserBuilder() = 
    member this.Bind(p, k) = 
        Parser(fun cs -> 
            match parse p cs with
            | Some(a), rest -> parse (k a) rest
            | _, _ -> None, [])
    
    member this.Return(a) = Parser(fun cs -> Some(a), cs)
    member this.Zero() = Parser(fun _ -> None, [])
    member this.ReturFrom(a) = a

let parser = new ParserBuilder()

(* Combine parsers with this operator *)
let (<|>) (p : Parser<'a>) q = 
    Parser(fun cs -> 
        match (parse (p) cs) with
        | Some(x), xs -> Some(x), xs
        | None, _ -> 
            match (parse (q) cs) with
            | None, xs -> None, xs
            | Some(x), xs -> Some(x), xs)

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
        | x :: xs -> x :: (result xs)
        | [] -> []
        | _ -> failwithf "Cannot find Token: %A" t
    
    let rec getRest (line : List<Token>) (full : List<Token>) = 
        match line with
        | x :: xs when x = full.Head -> getRest line.Tail full.Tail
        | [] when full.Length > 0 && full.Head = t -> full.Tail
        | _ -> full
    
    let line = result l
    line, getRest line l

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