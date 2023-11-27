module Solver

open WordDictionary
open PuzzleBoard

type SearchState =
    {
        trie: TrieNode;
        node: BoardNode;
    }
    member this.nextState = fun boardNode ->
        this.trie.nodeAt boardNode.value
        |> Option.map (fun (trieNode) ->
            { 
                trie = trieNode;
                node = boardNode
            })

let rec getWords state =
    let currentWord = match state.trie.value with
                        | RunnningWord _ -> []
                        | FullWord word -> [word]
    currentWord 
    @ (state.node.neighbors.Value
        |> List.choose state.nextState
        |> List.collect getWords)


let getAllWords (board: Board) (trie: TrieNode) =

    List.fold (fun wordSet node ->
        Set.union wordSet (Set.ofList (getWords { trie = trie; node = node }))) 
    <| Set.empty
    <| board.allNodes

let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getAllWords board trie
