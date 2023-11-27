module Solver

open WordDictionary
open PuzzleBoard

let getWordsFromStart board (trie: Node) coords =
    let rec getWordsHelper board (trie: Node) (x, y) =
        let neighbors = board.neighbors (x, y)

        let findEligibleNeighbors =
            neighbors
            |> List.choose (fun (letter, coord) ->
                trie.nodeAt letter
                |> Option.map (fun node -> (node, coord)))

        let currentWord = match trie.value with
                          | RunnningWord _ -> []
                          | FullWord word -> [word]
        currentWord 
        @ (findEligibleNeighbors
            |> List.collect (fun (nextBranch, coord) -> 
                getWordsHelper board nextBranch coord))

    
    let letter = board.letter coords
    trie.nodeAt letter
    |> Option.map (fun branch -> getWordsHelper board branch coords)
    |> Option.defaultValue []


let getAllWords board trie =
    let wordGetter = getWordsFromStart board trie

    List.fold (fun wordSet coord -> Set.union wordSet (Set.ofList (wordGetter coord))) 
        <| Set.empty
        <| board.allCoords


let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getAllWords board trie


    
