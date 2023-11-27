module WordDictionary

type NodeValue =
| RunnningWord of string
| FullWord of string

type TrieNode =
    {
        value: NodeValue;
        children: Map<char, TrieNode>
    }
    member this.nodeAt = this.children.TryFind

let buildTrie (words: string list) =
    let rec buildTrieHelper (words : string list) curIdx =
        words
        |> List.filter (fun word -> word.Length > curIdx)
        |> List.groupBy (fun word -> word.[curIdx])
        |> Map.ofList
        |> Map.map (fun _ group ->
            {
                value = match List.tryFind (fun (word : string) -> word.Length = curIdx + 1) group with
                        | Some word -> FullWord word
                        | None -> RunnningWord (group.Head.Substring(0, curIdx + 1));
                children = buildTrieHelper 
                    <| List.filter (fun word -> word.Length > curIdx + 1) group
                    <| curIdx + 1
            })
        
    {
        value = RunnningWord "";
        children = buildTrieHelper words 0
    }
