module Program

open Microsoft.CSharp
open System.CodeDom.Compiler
open System.IO
open System.Runtime.Serialization
open Parser
open Lexer
open Types
open Interpreter

(* Example program 1 (Variable declaration and assignment) *)
let prog1 = """
var x = 1 * 1 + 5 * 2;
var y = 3;

x <- y + 1;
y <- y + y;
var z = x * y;

print x;
print y;
print z;
"""
(* Example program 2 (Conditional statements, nested conditional statements, local scope) *)
let prog2 = """
var x = 5;
var y = 10;

if x * 5 > y * 2 {
    var z = 12;
    y <- 3;
} else {
    var z = true;
    x <- 100;
    if z {
        y <- 6;
    } else {
        y <- 2;
    };
};

print x;
print y;
"""
(* Example program 3 (While loop) *)
let prog3 = """
var x = 1;
var y = 100;
var z = false;

while y > x {
    x <- x + 1;
};

z <- true;

print x;
print y;
print z;
"""

(* Parse and execute the program, print the resulting memory / program state *)
[<EntryPoint>]
let main argv = 
    let tokens = (tokenize (prog3 |> Seq.toList))
    let t = Parser.doParse tokens
    do printfn "%A" t
    match t with
    | Some(program), _ ->
        do type_check program Map.empty
        do printfn "INTERPRET RESULT:"
        let int = interpret program Map.empty
        do printfn "MEMORY: %A" int
        do Compiler.generateSources program "Program"
        do Compiler.compileAndRun "Program"
    | None,_ -> do printfn ":*("
    0
