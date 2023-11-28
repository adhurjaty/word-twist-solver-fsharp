module SolverUtils

open Utils
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

let rec private getWordsHelper state =
    let currentWord = match state.trie.value with
                        | RunnningWord _ -> []
                        | FullWord word -> [word]
    currentWord 
    @ (state.node.neighbors.Value
        |> List.choose state.nextState
        |> List.collect getWordsHelper)

let getWords = memoize getWordsHelper
