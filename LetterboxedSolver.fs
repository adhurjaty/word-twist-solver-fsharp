module LetterboxedSolver

open WordDictionary
open PuzzleBoard
open LetterboxedBoard
open SolverUtils

type SearchedNode =
    {
        node: BoardNode;
        words: string list;
        remainingLetters: Set<char>;
    }

let private toCharSet (s: string) =
    s.ToCharArray() |> Set.ofSeq

let private getFewestWords (board: Board) (trie: TrieNode) =
    let letterNodeMap = board.allNodes
                        |> List.map (fun node -> (node.value, node))
                        |> Map.ofList
    let rec search (nodes: SearchedNode list) =
        match nodes with
        | head :: tail ->
            let node = head.node
            let startSearchState = {
                trie = trie;
                node = node;
            }
            startSearchState.nextState node
            |> Option.map (fun state ->
                let nextFrontier = 
                    getWords state
                    |> List.filter (fun word ->
                        (Set.intersect
                        <| toCharSet word
                        <| head.remainingLetters).Count > 0
                    )
                    |> List.map (fun word ->
                        {
                            node = letterNodeMap.[word.[word.Length - 1]];
                            words = head.words @ [word];
                            remainingLetters = head.remainingLetters - (toCharSet word)
                        })
                nextFrontier
                |> List.tryFind (fun node ->
                    node.remainingLetters.IsEmpty)
                |> Option.defaultWith (fun() -> search (
                    tail @ nextFrontier
                    |> List.sortBy (fun node ->
                        node.words.Length * 10 + node.remainingLetters.Count
                    )
                )))
            |> Option.defaultWith (fun () -> failwith "Invalid Trie")
        | [] -> failwith "No solution found"
    
    let allLetters = Set.ofSeq letterNodeMap.Keys
    let initNodes = board.allNodes
                    |> List.map (fun node ->
                        {
                            node = node;
                            words = [];
                            remainingLetters = allLetters
                        })
    (search initNodes).words
    

let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getFewestWords board trie
