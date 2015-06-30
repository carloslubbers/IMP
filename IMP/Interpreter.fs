module Interpreter

open Types

type Memory = Map<string, Value>

let restrict (variables : Set<string>) (memory : Memory) = 
    let newVariables = 
        [ for x in memory -> x.Key ]
        |> Set.ofList
    List.fold (fun m k -> Map.remove k m) memory (newVariables - variables |> Set.toList)

let rec eval_e (m : Memory) (e : Expr) : Value = 
    match e with
    | Const(v) -> v
    | Var(x) -> m.[x]
    | Plus(e1, e2) -> 
        match eval_e m e1, eval_e m e2 with
        | Value.Int(i1), Value.Int(i2) -> Value.Int(i1 + i2)
        | _ -> failwithf "Type error"
    | Times(e1, e2) -> 
        match eval_e m e1, eval_e m e2 with
        | Value.Int(i1), Value.Int(i2) -> Value.Int(i1 * i2)
        | _ -> failwithf "Type error"
    | GreaterThan(e1, e2) -> 
        match eval_e m e1, eval_e m e2 with
        | Value.Int(i1), Value.Int(i2) -> 
            match i1 > i2 with
            | true -> Value.Bool(true)
            | false -> Value.Bool(false)
        | _ -> failwithf "Type error"

and eval_s (m : Memory) (s : Term) : Memory = 
    match s with
    | VarDecl(x, e) -> m.Add(x, eval_e m e)
    | VarAssign(x, e) -> 
        if m.ContainsKey(x) then m.Add(x, eval_e m e)
        else failwith "Undeclared variable"
    | CodeBlock(x) -> 
        match x with
        | x :: xs -> 
            let m' = eval_s m x
            eval_s m' (CodeBlock xs)
        | [] -> m
    | If(c, t, e) -> 
        let vars = 
            [ for x in m -> x.Key ]
            |> Set.ofList
        match eval_e m c with
        | Value.Bool(true) -> 
            let m' = eval_s m t
            restrict vars m'
        | Value.Bool(false) -> 
            let m' = eval_s m e
            restrict vars m'
        | _ -> failwithf "Conditional statement must return Boolean value"
    | While(c, d) -> 
        let vars = 
            [ for x in m -> x.Key ]
            |> Set.ofList
        match eval_e m c with
        | Value.Bool(true) -> 
            let m' = eval_s m d
            match eval_e m' c with
            | Value.Bool(true) -> eval_s m' (While(c, d))
            | Value.Bool(false) -> m'
            | _ -> failwithf "Conditional statement must return Boolean value"
        | Value.Bool(false) -> m
        | _ -> failwithf "Conditional statement must return Boolean value"
    | Print(e) -> 
        let result = eval_e m e
        printfn "%A" result
        m
    | EOF -> m

let rec type_check_s (s : Term) (ctxt : Map<string, Type>) : Map<string, Type> = 
    match s with
    | VarDecl(x, e) -> 
        let e_T = get_type e ctxt
        if ctxt.ContainsKey(x) then failwith "Already declared variable"
        let ctxt' = Map.add x e_T ctxt
        ctxt'
    | VarAssign(x, e) -> 
        let e_T = get_type e ctxt
        let x_T = ctxt.[x]
        if x_T <> e_T then failwith "Type mismatch"
        ctxt
    | If(c, t, e) -> 
        if get_type c ctxt <> Type.Bool then failwith "Only boolean conditions allowed in if. LOL."
        let ctxt_t = type_check_s t ctxt
        let ctxt_e = type_check_s e ctxt
        ctxt
    | While(c, d) -> 
        if get_type c ctxt <> Type.Bool then failwith "Only boolean conditions allowed in while. LOL."
        let ctxt_d = type_check_s d ctxt
        ctxt
    | CodeBlock(x :: xs) -> 
        let a = type_check_s x ctxt
        type_check_s (CodeBlock(xs)) a
    | CodeBlock([]) -> ctxt
    | Print(_) -> ctxt
    | EOF -> ctxt

and get_type (e : Expr) (ctxt : Map<string, Type>) : Type = 
    match e with
    | Const(Value.Int(_)) -> Type.Int
    | Const(Value.Bool(_)) -> Type.Bool
    | Var(x) -> ctxt.[x]
    | GreaterThan(e1, e2) -> 
        if get_type e1 ctxt <> Type.Int || get_type e2 ctxt <> Type.Int then failwith "Cannot compare non-int values"
        Type.Bool
    | Plus(e1, e2) -> 
        if get_type e1 ctxt <> Type.Int || get_type e2 ctxt <> Type.Int then failwith "Cannot add non-int values"
        Type.Int
    | Times(e1, e2) -> 
        if get_type e1 ctxt <> Type.Int || get_type e2 ctxt <> Type.Int then failwith "Cannot multiply non-int values"
        Type.Int

(* Perform Type checking on the program *)
let rec type_check (p : List<Term>) (ctxt : Map<string, Type>) = 
    match p with
    | x :: xs -> 
        let ctxt' = type_check_s x ctxt
        type_check xs ctxt'
    | [] -> ()

let rec interpret (p : List<Term>) (m : Map<string, Value>) = 
    match p with
    | x :: xs -> 
        let m' = eval_s m x
        // do printfn "%A" m'
        interpret xs m'
    | [] -> m
