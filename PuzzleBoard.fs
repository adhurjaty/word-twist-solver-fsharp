
module PuzzleBoard

type Element =
| Border
| Tile of char

type Board = Element[,]

type Point = int * int

let createBoard (lines : string list) : Board =
    let createBorder count = 
        List.replicate count Border

    let boardLength = (List.tryHead lines).Value.Length + 2
    let boardHeight = lines.Length + 2

    let elementArrays = lines
                        |> List.map (fun line -> line.ToCharArray() 
                                                |> Array.map Tile)
                        |> List.toArray

    Array2D.init boardLength boardHeight (fun x y ->
        match x with
        | i when i = 0 || i = boardLength - 1 ->
            Border
        | i ->
            match y with
            | j when j = 0 || j = boardHeight - 1 ->
                Border
            | j ->
                elementArrays.[j-1].[i-1])


let private getSurroundingLettersKernel kernelSize (board : Board) (x, y) =
    let offset = kernelSize / 2

    List.allPairs [x + 1 - offset .. x + 1 + offset] [y + 1 - offset .. y + 1 + offset]
    |> List.filter (fun (i, j) -> i - 1 <> x || j - 1 <> y)
    |> List.map (fun (i, j) -> board.[i,j], (i, j))
    |> List.choose (fun (element, (i, j)) ->
        match element with
        | Tile c -> Some (c, (i-1, j-1))
        | Border -> None)


let getSurroundingLetters = getSurroundingLettersKernel 3

let getBoardLetter (board : Board) (x, y) =
    let width = Array2D.length1 board
    let height = Array2D.length2 board
    match x + 1 with
    | i when i >= width-1 || i < 1 ->
        failwith "X index out of bounds"
    | i ->
        match y + 1 with
        | j when j >= height-1 || j < 1 ->
            failwith "Y index out of bounds"
        | j ->
            match board.[i,j] with
            | Tile c -> c
            | Border -> failwith "Cannot get letter from border (should not happen)"


let getAllCoords board =
    List.allPairs [0 .. (Array2D.length1 board - 3)] [0 .. (Array2D.length2 board - 3)]
