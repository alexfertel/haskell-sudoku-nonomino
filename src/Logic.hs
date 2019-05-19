module Logic where 
    import Data.Maybe
    import Data.List

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

    data Nonomino = Nonomino {
        nid :: Int,
        points :: [(Int, Point)]
    } deriving Show

    justPoints :: Nonomino -> [Point]
    justPoints nono = map snd (points nono)

    sqsToList :: [Square] -> [Point]
    sqsToList = map (\square -> (row square, col square))

    nonoToSquares :: Nonomino -> [Square]
    nonoToSquares nono = sqs
        where sqs = map (\(v, (i, j)) -> Square i j (Just (nid nono)) (st v)) (points nono)
              st 0 = Left [1..9]
              st i = Right i

    compute :: Point -> Nonomino -> Nonomino
    compute (x, y) nono = Nonomino (nid nono) pts'
        where pts' = map (\(v, (i, j)) -> (v, (x + i, y + j))) (points nono)

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
    collides nono board = any (\point -> occupied point board) (justPoints nono)

    invalid :: Nonomino -> Bool
    invalid nono = any (\(x, y) -> x < 0 || 8 < x || y < 0 || 8 < y) (justPoints nono)  

    updateBoard :: [Square] -> Board -> Board
    updateBoard [] board = board
    updateBoard (s:ss) board = updateBoard ss board'
        where board' = updateSquare s board

    updateSquare :: Square -> Board -> Board    
    updateSquare sq board = Board [ if row sq == row sq' && col sq == col sq' then sq else sq' | sq' <- (squares board) ]

    chunks :: Int -> [a] -> [[a]]
    chunks _ [] = []
    chunks size xs = [(take size xs)] ++ (chunks size (drop size xs))

    -- linesToTuples :: [String] -> [[(Int, Int, Int)]]
    -- linesToTuples l = groupBy (\(i1, _, _) (i2, _, _) -> i1 == i2) (sort tuples) 
    --     where matrix = map words l
    --           tuples = [(read $ matrix !! i !! j, i, j) | i <- [0..8], j <- [0..8]]

    -- tuplesToNonos :: [[(Int, Point)]] -> [Nonomino]
    -- tuplesToNonos tuples = [Nonomino i (unindexed !! i) | i <- [1..length unindexed]]
    --     where unindexed = map (\l@((i, _):_) -> map (\(_, (x, y)) -> (x, y)) l) tuples

    

