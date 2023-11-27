module Solver

open WordDictionary
open PuzzleBoard

let rec getWords (trie: TrieNode) (node: BoardNode) =
    let currentWord = match trie.value with
                        | RunnningWord _ -> []
                        | FullWord word -> [word]
    currentWord 
    @ (node.neighbors.Value
        |> List.choose (fun (neighborNode) ->
            trie.nodeAt neighborNode.value
            |> Option.map (fun (trieNode) -> trieNode, neighborNode))
        |> List.collect (fun (trieNode, neighborNode) -> 
            getWords trieNode neighborNode))


let getAllWords (board: Board) trie =
    let wordGetter = getWords trie

    List.fold (fun wordSet node -> Set.union wordSet (Set.ofList (wordGetter node))) 
    <| Set.empty
    <| board.allNodes

let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getAllWords board trie


    
