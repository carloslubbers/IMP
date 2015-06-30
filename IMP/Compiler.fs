module Compiler

open Types
open Microsoft.CSharp
open System.CodeDom.Compiler
open System.IO
open System.Runtime.Serialization
open System.Reflection

(* Translate an IMP expression to C# *)
let rec translate_e (e : Expr) : string = 
    match e with
    | Const(v) -> 
        match v with
        | Value.Int(x) -> string x
        | Value.Bool(true) -> "true"
        | Value.Bool(false) -> "false"
    | Var(x) -> x
    | Plus(e1, e2) -> (translate_e e1) + "+" + (translate_e e2)
    | Times(e1, e2) -> (translate_e e1) + "*" + (translate_e e2)
    | GreaterThan(e1, e2) -> (translate_e e1) + ">" + (translate_e e2)

(* Translate an IMP term to C# *)
and translate_s (s : Term) : string = 
    match s with
    | VarDecl(x, e) -> "var " + x + " = " + (translate_e e) + "; "
    | VarAssign(x, e) -> x + "=" + (translate_e e) + "; "
    | CodeBlock(x) -> 
        match x with
        | x :: xs -> 
            let m' = translate_s x
            m' + translate_s (CodeBlock xs)
        | [] -> ""
    | If(c, t, e) -> "if (" + (translate_e c) + ") {" + (translate_s t) + "} else {" + (translate_s e) + "} "
    | While(c, d) -> "while (" + (translate_e c) + ") {" + (translate_s d) + "} "
    | Print(e) -> "Console.WriteLine(" + (translate_e e) + "); "
    | EOF -> ""

(* Translate the program *)
let generateSources (l : List<Term>) s = 
    let rec generateLines (t : List<Term>) = 
        match t with
        | x :: xs -> (translate_s x) + (generateLines xs)
        | [] -> ""
    
    let gen = "using System; namespace Generated {class Program {static void Main() {" + (generateLines l) + "}}}"
    do printfn "\nGENERATED:\n%s\n" gen
    File.WriteAllText(s + ".cs", gen)

(* Generate a C# source (path s) and an executable file *)
let compileAndRun(s) = 
    let generatedPath = s + ".cs" // path containing C# sources to compile
    let code = File.ReadAllLines(generatedPath)
    let dll_name = "Program" // path to C# DLL to generate

    // Build the compiler object and its parameters
    let args = new System.Collections.Generic.Dictionary<string, string>()
    do args.Add("CompilerVersion", "v4.5")
    let provider = CodeDomProvider.CreateProvider("CSharp")
    let parameters = new CompilerParameters()
    do parameters.OutputAssembly <- s + ".dll"
    do parameters.GenerateExecutable <- true
    do parameters.CompilerOptions <- @"/out:"+ s+ ".exe"

    // Compile the code into the executable
    let results = provider.CompileAssemblyFromSource(parameters, code)
    if results.Errors.HasErrors then 
        for error in results.Errors do
            if error.IsWarning |> not then do printfn "%s at %d: %s" error.FileName error.Line error.ErrorText
