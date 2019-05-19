module Logic where 
    import Data.Maybe
    import Data.List
    import Sudoku

    data Board = Board {
        squares :: [Square]
    }

    data Square = Square {
        row :: Int,
        col :: Int,
        nono :: Maybe Int,
        status :: Either [Int] Int
    } deriving (Show, Eq)

    instance Ord Square where
        (Square r1 c1 _ _) `compare` (Square r2 c2 _ _) = (r1 `compare` r2) `mappend` (c1 `compare` c2)

    type Point = (Int, Int)

    data Nonomino = Nonomino Int [Point] deriving Show

    sqsToList :: [Square] -> [Point]
    sqsToList = map (\square -> (row square, col square))

    nonoToSquares :: Nonomino -> [Square]
    nonoToSquares (Nonomino nid pts) = sqs
        where sqs = map (\(i, j) -> Square i j (Just nid) (Left [1..9])) pts

    compute :: Point -> Nonomino -> Nonomino
    compute (x, y) (Nonomino nid pts) = Nonomino nid pts'
        where pts' = map (\(i, j) -> (x + i, y + j)) pts

    emptyBoard :: Board
    emptyBoard = Board [Square x y Nothing (Left [1..9]) | x <- [0..8], y <- [0..8]]

    assemble :: [Nonomino] -> [Board]
    assemble ns = map fromJust $ filter (isJust) [tryMatch permutation ns emptyBoard | permutation <- permutations [0..8]]

    tryMatch :: [Int] -> [Nonomino] -> Board -> Maybe Board
    tryMatch [] _ board = Just board
    tryMatch (perm:perms) ns board
        | isNothing board' = Nothing
        | otherwise = tryMatch perms ns $ fromJust board'
        where board' = tryPlace (ns !! perm) board

    tryPlace :: Nonomino -> Board -> Maybe Board
    tryPlace nono board
        | (invalid nono') || (collides nono' board) = Nothing
        | otherwise = Just board'
        where nono' = compute (firstEmpty (squares board)) nono
              board' = updateBoard sqs board
              sqs = nonoToSquares nono'

    occupied :: Point -> Board -> Bool
    occupied (x, y) board = elem (x, y) $ map (\square -> (row square, col square)) $ filled $ squares board

    filled :: [Square] -> [Square]
    filled = filter (isJust . nono)

    empty :: [Square] -> [Square]
    empty = filter (isNothing . nono)

    firstEmpty :: [Square] -> Point
    firstEmpty sqs = head $ sort $ map (\square -> (row square, col square)) $ empty sqs

    collides :: Nonomino -> Board -> Bool
    collides (Nonomino _ pts) board = any (\point -> occupied point board) pts

    invalid :: Nonomino -> Bool
    invalid (Nonomino _ pts) = any (\(x, y) -> x < 0 || 8 < x || y < 0 || 8 < y) pts  

    updateBoard :: [Square] -> Board -> Board
    updateBoard [] board = board
    updateBoard (s:ss) board = updateBoard ss board'
        where board' = updateSquare s board

    updateSquare :: Square -> Board -> Board    
    updateSquare sq board = Board [ if row sq == row sq' && col sq == col sq' then sq else sq' | sq' <- (squares board) ]
        
    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks size xs = [(take size xs)] ++ (chunks size (drop size xs))
    
    linesToTuples :: [String] -> [[(Int, Int, Int)]]
    linesToTuples l = groupBy (\(i1, _, _) (i2, _, _) -> i1 == i2) (sort tuples) 
        where matrix = map words l
              tuples = [(read $ matrix !! i !! j, i, j) | i <- [0..8], j <- [0..8]]

    tuplesToNonos :: [[(Int, Int, Int)]] -> [Nonomino]
    tuplesToNonos = map (\l@((i, _, _):_) -> Nonomino i (map (\(_, x, y) -> (x, y)) l))

    

