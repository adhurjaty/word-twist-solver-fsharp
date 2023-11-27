module LetterboxedBoard

open PuzzleBoard

let createBoard (lines : string list) : Board =
    let elementArrays = lines
                        |> List.map (fun line -> line.ToCharArray())
                        |> List.toArray
    let rec buildGraph letter rowIdx =
        {
            value = letter;
            neighbors = lazy ([0 .. elementArrays.Length - 1]
                        |> List.filter (fun i -> i <> rowIdx) 
                        |> List.collect (fun i ->
                            elementArrays.[i]
                            |> Array.map (fun nextLetter -> buildGraph nextLetter i)
                            |> Array.toList
                        ))
        }

    {
        allNodes = [0 .. elementArrays.Length - 1]
                    |> List.collect (fun i ->
                        elementArrays.[i]
                        |> Array.map (fun nextLetter ->
                            buildGraph nextLetter i)
                        |> Array.toList
                    )
    }
    