module LexerMonad
open System

(* Build the Lexer Monad *)
type Lexer<'a> = 
    | Lexer of (char list -> (Option<'a> * List<char>))

let parse (Lexer p) = p

type LexerBuilder() = 
    member this.Bind(p, k) = 
        Lexer(fun cs -> 
            match parse p cs with
            | Some(a), rest -> parse (k a) rest
            | _, _ -> None, [])
    
    member this.Return(a) = Lexer(fun cs -> Some(a), cs)
    member this.Zero() = Lexer(fun _ -> None, [])
    member this.ReturFrom(a) = a

let lexer = new LexerBuilder()

(* Combine lexers with this operator *)
let (<|>) (p : Lexer<'a>) q = 
    Lexer(fun cs -> 
        match (parse (p) cs) with
        | Some(x), xs -> Some(x), xs
        | None, _ -> 
            match (parse (q) cs) with
            | None, xs -> None, xs
            | Some(x), xs -> Some(x), xs)

let tcl s = s |> Seq.toList

(* Parse chars '0' - '9' to their Int32 values *)
let parseInt = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsDigit(x) then Some(Int32.Parse(x.ToString())), xs
            else None, xs
        | _ -> None, [])

(* Only parse 'a' - 'z' & 'A' - 'Z' *)
let parseLetter = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsLetter(x) then Some(x), xs
            else None, xs
        | [] -> None, [])

(* Only parse '0'-'9' to their Char values *)
let parseDigit = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsDigit(x) then Some(x), xs
            else None, xs
        | [] -> None, [])

(* Only parse symbols *)
let parseSymbol = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsLetter(x) = false && Char.IsNumber(x) = false && Char.IsWhiteSpace(x) = false then Some(x), xs
            else None, xs
        | [] -> None, [])

(* Parse whitespace *)
let parseWhitespace = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsWhiteSpace(x) then Some(x), xs
            else None, xs
        | [] -> None, [])

(* Remove whitespace *)
let removeWhitespace = 
    Lexer(fun cs -> 
        match cs with
        | x :: xs -> 
            if Char.IsWhiteSpace(x) || x = '\n' then Some(x), xs
            else None, x :: xs
        | [] -> None, [])

(* Parse two ints *)
let parseTwoInts = lexer { let! first = parseInt
                           let! second = parseInt
                           return (first, second) }
(* Parse only a specific character *)
let rec character c =
    Lexer(fun cs ->
        match cs with
        | x :: xs ->
            if System.Char.ToLower(c).Equals(x) then
                Some x, xs
            else
                None, xs
        | [] -> None, cs)

(* Parse only a specific word *)                        
let rec parseWord s = lexer { 
                        match s with
                        | x::xs ->
                         let! c = character x
                         let! cs = parseWord xs
                         return c::cs
                        | [] -> 
                         return []
                      } 

(* Keep running the parser until it fails *)
let rec parseMany p = lexer { let! x = p
                              let! xs = parseMany p
                              return x :: xs } <|> lexer { return [] }
let rec parseMany' p = lexer { let! x = p
                               let! xs = parseMany p
                               return x :: xs }

(* Test the parsers *)
let test() = 
    do printfn "Parsed one integer          : %A" (parse parseInt [ '0'; '1'; '2' ])
    do printfn "Parsed two integers         : %A" (parse parseTwoInts [ '0'; '1'; '2' ])
    do printfn "Parse a letter              : %A" (parse parseLetter [ 'a'; 'b'; 'c' ])
    do printfn "Parse to letter or symbol   : %A" (parse (parseLetter <|> parseSymbol) [ '+'; 'a'; '1' ])
    do printfn "Failed to parse let/sym     : %A" (parse (parseLetter <|> parseSymbol) [ '0'; '+'; '1' ])
    do printfn "Failed to parse one integer : %A" (parse parseInt [ 'a'; 'b'; 'c' ])
    do printfn "Failed to parse a letter    : %A" (parse parseLetter [ '+'; 'b'; '4' ])
    do printfn "Parse many integers         : %A" (parse (parseMany (parseInt)) [ '1'; '2'; '3'; 'a'; '4' ])
    do printfn "Parse many let & sym        : %A" 
           (parse (parseMany (parseLetter <|> parseSymbol)) [ 'a'; 'b'; ' '; '-'; '4' ])
    do printfn "Parse words                 : %A" (parse (parseMany (parseWord (List.ofSeq "hoi"))) (List.ofSeq "hoihoidoeihoihoi"))
