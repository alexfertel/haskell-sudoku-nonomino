module Sudoku where
    import Logic
    import Data.List
    import Data.Maybe
    import Data.Either

    cleanBoard :: Board -> Board
    cleanBoard (board@(Board sqs)) = sideEffects rights board
        where rights = getRightSquares sqs

    sideEffects :: [Square] -> Board -> Board
    sideEffects [] board = board
    sideEffects (s:ss) board = sideEffects ss board'
        where propagate sq@(Square r c _ (Right d)) =
                let newRow = updateSquares d $ getLeftSquares $ getRow r board
                    newCol = updateSquares d $ getLeftSquares $ getCol c board
                    newBox = updateSquares d $ getLeftSquares $ getBox sq board
                in (newRow ++ newCol ++ newBox)
              board' = updateBoard (propagate s) board

    updateSquares :: Int -> [Square] -> [Square]
    updateSquares d sqs = map (\(Square srow scol snono (Left ds)) -> Square srow scol snono (Left (ds \\ [d])) ) (getLeftSquares sqs)

    getLeftSquares :: [Square] -> [Square]
    getLeftSquares sqs = filter (isLeft . status) sqs
    
    getRightSquares :: [Square] -> [Square]
    getRightSquares sqs = filter (isRight . status) sqs

    getRow :: Int -> Board -> [Square]
    getRow i board = filter ((== i) . row) $ squares board

    getCol :: Int -> Board -> [Square]
    getCol i board = filter ((== i) . col) $ squares board
    
    getBox :: Square -> Board -> [Square]
    getBox sq board = filter (\sq' -> (fromJust (nono sq')) == (fromJust (nono sq))) $ squares board


    setSquare :: Square -> Board -> Board
    setSquare sq@(Square srow scol (Just snono) (Right d)) (Board sqs) = Board (map set sqs)
        where set osq@(Square orow ocol (Just onono) ds) = 
                if ocol == scol && orow == srow then sq
                else if ocol == scol || orow == srow || onono == snono
                    then (Square orow ocol (Just onono) (sub ds))
                else osq
              sub (Left ds) = Left (ds \\ [d])
              sub dd = dd
    
    -- Given an initial Board return all the possible solutions starting
    -- from that Board.
    -- Note, this all happens in the list monad and makes use of lazy evaluation
    -- to avoid work.  Using the list monad automatically handles all the backtracking
    -- and enumeration of solutions.
    solveThemAll :: Board -> [Board]
    solveThemAll brd =
        case getLeftSquares (squares brd) of
            [] -> return brd            -- Nothing unsolved remains, we are done.
            sqs -> do
                -- Sort the unsolved Squares by the ascending length of the possible
                -- digits.  Pick the first of those so we always solve forced Squares
                -- first.
                let Square c r b (Left ds) : _ = sortBy leftLen sqs
                    leftLen (Square _ _ _ (Left ds1)) (Square _ _ _ (Left ds2)) = compare (length ds1) (length ds2)
                sq <- [ Square c r b (Right d) | d <- ds ] -- Try all possible moves
                solveThemAll (setSquare sq brd) -- And solve the extended Board.

