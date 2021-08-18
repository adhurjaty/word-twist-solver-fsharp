module Solver

open IntermediateRepresentation.WordDictionary
open IntermediateRepresentation.PuzzleBoard


let getWordsFromStart board trie coords =
    let getLetter = getBoardLetter board
    let getNeighbors = getSurroundingLetters board

    let rec getWordsHelper board trie (x, y) =
        let neighbors = getNeighbors (x, y)

        let findEligibleNeighbors branches =
            branches
            |> List.choose (fun nextBranch -> 
                neighbors
                |> List.tryFind (fun (c, _) -> 
                    match nextBranch.node with
                    | Letter c' | WordTerminal (c', _) when c' = c -> true
                    | _ -> false)
                |> Option.map (fun neighbor -> (nextBranch, neighbor)))

        let currentWord = match trie.node with
                            | Root | Letter _ -> []
                            | WordTerminal (_, word) -> [word]
        currentWord 
        @ (trie.branches
            |> findEligibleNeighbors
            |> List.collect (fun (nextBranch, (_, coord)) -> 
                getWordsHelper board nextBranch coord))

    
    let letter = getLetter coords
    getBranch trie letter
    |> Option.map (fun branch -> getWordsHelper board branch coords)
    |> Option.defaultValue []


let getAllWords board trie =
    let wordGetter = getWordsFromStart board trie

    List.fold (fun wordSet coord -> Set.union wordSet (Set.ofList (wordGetter coord))) 
        <| Set.empty
        <| getAllCoords board


let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getAllWords board trie


    
