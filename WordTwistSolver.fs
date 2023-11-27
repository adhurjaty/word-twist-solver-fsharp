module WordTwistSolver

open WordDictionary
open PuzzleBoard
open WordTwistBoard
open SolverUtils

let getAllWords (board: Board) (trie: TrieNode) =
    List.fold (fun wordSet node ->
        Set.union wordSet (Set.ofList (getWords { trie = trie; node = node }))) 
    <| Set.empty
    <| board.allNodes

let solve boardWords dictWords =
    let board = createBoard boardWords
    let trie = buildTrie dictWords

    getAllWords board trie
