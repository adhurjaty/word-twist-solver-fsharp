
module PuzzleBoard

type BoardNode =
    {
        value: char;
        neighbors: Lazy<List<BoardNode>>;
    }

type Board =
    {
        allNodes: BoardNode list;
    }
