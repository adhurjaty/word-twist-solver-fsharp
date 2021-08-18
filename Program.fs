// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO

open Solver

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let words = File.ReadAllLines "./words.txt"
            |> Array.toList

let boardInput = File.ReadAllLines "./board.txt"
                |> Array.toList


[<EntryPoint>]
let main argv =
    printf "%s" (String.concat "\n" <| solve boardInput words)
    0 // return an integer exit code