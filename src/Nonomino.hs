module Nonomino where 
    import Data.Maybe
    import Data.List

    -- Represents a board
    data Board = Board {
        squares :: [Square]  -- Squares of the board
    }

    -- Represents a square of a board
    data Square = Square {
        row :: Int,                 -- The row :).
        col :: Int,                 -- The column (:.
        nono :: Maybe Int,          -- The nonomino Identifier, which starts as Nothing.
        -- If this is `Right`, it has a value, if not this is a list of possibilities.
        status :: Either [Int] Int  
    } deriving (Show, Eq)

    -- You may compare to Squares, they define a partial order.
    instance Ord Square where
        (Square r1 c1 _ _) `compare` (Square r2 c2 _ _) = (r1 `compare` r2) `mappend` (c1 `compare` c2)

    -- Synonim for pairs
    type Point = (Int, Int)

    -- Represents a nonomino as a list of 'displacements',
    -- we use a function that computes translations, so
    -- we can compute the points of a nonomino, given a square
    -- in the board
    data Nonomino = Nonomino {
        nid :: Int,  -- Handy property
        -- Represents the displacements and what digit is in each
        -- square of the Nonomino. Zero(0) represents empty square.
        points :: [(Int, Point)]
    } deriving Show

    -- Helper to get the Points of a Nonomino
    justPoints :: Nonomino -> [Point]
    justPoints nono = map snd (points nono)

    -- Takes a Nonomino(after being put on the board ideally) and
    -- converts it to a Square. If a position was empty(0), then
    -- its `status` would be `Left [1..9]`, else `Right i`. 
    nonoToSquares :: Nonomino -> [Square]
    nonoToSquares nono = sqs
        where sqs = map (\(v, (i, j)) -> Square i j (Just (nid nono)) (st v)) (points nono)
              st 0 = Left [1..9]
              st i = Right i

    -- Given `Point` and a `Nonomino`, computes
    -- the translated `Nonomino`.
    compute :: Point -> Nonomino -> Nonomino
    compute (x, y) nono = Nonomino (nid nono) pts'
        where pts' = map (\(v, (i, j)) -> (v, (x + i, y + j))) (points nono)

    -- The empty initial board
    emptyBoard :: Board
    emptyBoard = Board [Square x y Nothing (Left [1..9]) | x <- [0..8], y <- [0..8]]

    -- Given a list of `Nonomino` returns all of the possible
    -- board that come from assembling the nonominos together
    -- as a 9 x 9 square. It tries each permutation of [0..8]
    -- as the order of the nonominos to be placed.
    assembleThemAll :: [Nonomino] -> [Board]
    assembleThemAll ns = map fromJust $ filter (isJust) [try permutation | permutation <- permutations [0..8]]
        where try permutation = tryAssemble permutation ns emptyBoard
    
    -- Given a permutation, a list of nonominos and a board,
    -- try to assemble the nonominos together and return the
    -- resulting list of squares(board).
    tryAssemble :: [Int] -> [Nonomino] -> Board -> Maybe Board
    tryAssemble [] _ board = Just board
    tryAssemble (perm:perms) ns board
        | isNothing board' = Nothing
        | otherwise = tryAssemble perms ns $ fromJust board'
        where board' = tryPlace (ns !! perm) board

    -- Given a `Nonomino` and a `Board`, try to put the
    -- nonomino in the first square that is empty from
    -- top to bottom and left to right.
    tryPlace :: Nonomino -> Board -> Maybe Board
    tryPlace nono board
        | (invalid nono') || (collides nono' board) = Nothing
        | otherwise = Just board'
        where nono' = compute (firstEmpty (squares board)) nono
              board' = updateBoard sqs board
              sqs = nonoToSquares nono'

    -- Given a point and a board, tells if that point
    -- in the given board is already filled by a nonomino.
    occupied :: Point -> Board -> Bool
    occupied (x, y) board = elem (x, y) $ map (\square -> (row square, col square)) $ filled $ squares board

    -- Given a list of squares, return the ones
    -- that already have a nonomino associated.
    filled :: [Square] -> [Square]
    filled = filter (isJust . nono)

    -- Given a list of squares, return the ones
    -- that still don't have a nonomino associated.
    empty :: [Square] -> [Square]
    empty = filter (isNothing . nono)

    -- Given a list of squares, return the
    -- first square that is empty from
    -- top to bottom and left to right.
    firstEmpty :: [Square] -> Point
    firstEmpty sqs = head $ sort $ map (\square -> (row square, col square)) $ empty sqs

    -- Given a nonomino and a board, tells if any point of
    -- the nonomino overlaps another nonomino.
    collides :: Nonomino -> Board -> Bool
    collides nono board = any (\point -> occupied point board) (justPoints nono)

    -- Given a nonomino, tells if it can be placed inside the board
    -- i.e., is inside the boundaries of the board.
    invalid :: Nonomino -> Bool
    invalid nono = any (\(x, y) -> x < 0 || 8 < x || y < 0 || 8 < y) (justPoints nono)  

    -- Given a list of squares and a board, returns a
    -- new board with the squares of the list as
    -- new squares (replaces the ones currently in the board).
    updateBoard :: [Square] -> Board -> Board
    updateBoard [] board = board
    updateBoard (s:ss) board = updateBoard ss board'
        where board' = updateSquare s board

    -- Given a square and a board, returns a new board
    -- with the new square replacing the one that matches.
    updateSquare :: Square -> Board -> Board    
    updateSquare sq board = Board [ if row sq == row sq' && col sq == col sq' then sq else sq' | sq' <- (squares board) ]

    -- Given an Int 'i' and a list, returns lists of size 'i'
    -- Example:
    -- > let l = [1, 2, 3, 4, 5, 6, 7, 8, 9]
    -- > show $ chunks 3 l
    -- [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks size xs = [(take size xs)] ++ (chunks size (drop size xs))