module WordTwistBoard

open PuzzleBoard

let private getCoordsKernel kernelSize (boardWidth, boardHeight) coord =
    let offset = kernelSize / 2
    let x, y = coord

    List.allPairs [x - offset .. x + offset] [y - offset .. y + offset]
    |> List.filter (fun loc -> 
        let i, j = loc
        loc <> coord && i >= 0 && j >= 0 && i < boardWidth && j < boardHeight)

let private getSurroundingCoords = getCoordsKernel 3

let createBoard (lines : string list) : Board =
    let boardLength = (List.tryHead lines).Value.Length
    let boardHeight = lines.Length

    let elementArrays = lines
                        |> List.map (fun line -> line.ToCharArray())
                        |> List.toArray

    let allCoords = List.allPairs [0 .. boardLength - 1] [0 .. boardHeight - 1]
    let board = Array2D.init boardLength boardHeight (fun x y ->
        elementArrays.[y].[x])
    
    let rec buildGraph (x, y) =
        {
            value = board.[x, y];
            neighbors = lazy (getSurroundingCoords
                                <| (board.GetLength(0), board.GetLength(1))
                                <| (x, y)
                             |> List.map buildGraph)
        }

    {
        allNodes = List.map buildGraph allCoords
    }
