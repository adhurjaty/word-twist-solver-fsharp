// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System.IO

// open WordTwistSolver
open LetterboxedSolver

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

let words = File.ReadAllLines "./words.txt"
            |> Array.toList

// let boardInput = File.ReadAllLines "./board.txt"
//                 |> Array.toList
let boardInput = File.ReadAllLines "letterboxed.txt"
                |> Array.toList


[<EntryPoint>]
let main argv =
    let solvedWords = solve boardInput words
    printfn "%s" (String.concat "\n" solvedWords)
    printfn "%i" solvedWords.Length
    // printfn "%i" solvedWords.Count
    0 // return an integer exit code