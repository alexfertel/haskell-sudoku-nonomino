module Sudoku where
    import Nonomino
    import Data.List
    import Data.Maybe
    import Data.Either

    -- Takes a just-assembled board where all of the
    -- `square status` that aren't `Right`, are
    -- `Left [1..9]` and sets them to their proper
    -- possibilities.
    cleanBoard :: Board -> Board
    cleanBoard (board@(Board sqs)) = sideEffects rights board
        where rights = getRightSquares sqs

    -- Takes a list of `Right` and a `Board` and
    -- returns all of the squares that need to update
    -- their values. For example, if there is a `Right 7`
    -- square in the list, then in that square's row, col
    -- and box (nonomino) can be another square that has a
    -- status of `Right 7`. The `List` `Monad` is used
    -- here as a means of example for the latter use in the
    -- `solveThemAll` function, the same result could be
    -- achieved with a code like:
    --    where propagate sq@(Square r c _ (Right d)) =
    --            let newRow = updateSquares d $ getLeftSquares $ getRow r board
    --                newCol = updateSquares d $ getLeftSquares $ getCol c board
    --                newBox = updateSquares d $ getLeftSquares $ getBox sq board
    --            in (newRow ++ newCol ++ newBox)
    sideEffects :: [Square] -> Board -> Board
    sideEffects [] board = board
    sideEffects (s:ss) board = sideEffects ss board'
        where propagate sq@(Square r c _ (Right d)) = do
                let newRow = updateSquares d $ getLeftSquares $ getRow r board
                    newCol = updateSquares d $ getLeftSquares $ getCol c board
                    newBox = updateSquares d $ getLeftSquares $ getBox sq board
                (newRow ++ newCol ++ newBox)
              board' = updateBoard (propagate s) board

    -- Takes an int 'd' and a list of squares and returns
    -- a new list of squares with the `status` updated
    -- to be a new `Left ds` where 'd' does not belong to `ds` 
    updateSquares :: Int -> [Square] -> [Square]
    updateSquares d sqs = map (\(Square srow scol snono (Left ds)) -> Square srow scol snono (Left (ds \\ [d])) ) (getLeftSquares sqs)

    -- Takes a list of them and returns the squares that
    -- have a status of `Left`
    getLeftSquares :: [Square] -> [Square]
    getLeftSquares sqs = filter (isLeft . status) sqs
    
    -- Takes a list of them and returns the squares that
    -- have a status of `Right`
    getRightSquares :: [Square] -> [Square]
    getRightSquares sqs = filter (isRight . status) sqs

    -- Returns the i-th row of a board 
    getRow :: Int -> Board -> [Square]
    getRow i board = filter ((== i) . row) $ squares board

    -- Returns the i-th column of a board 
    getCol :: Int -> Board -> [Square]
    getCol i board = filter ((== i) . col) $ squares board
    
    -- Takes a `Square` and a `Board` and returns the list
    -- of squares that share the same nonomino
    getBox :: Square -> Board -> [Square]
    getBox sq board = filter (\sq' -> (fromJust (nono sq')) == (fromJust (nono sq))) $ squares board

    -- This function does some heavy-lifting, it takes a `Square`
    -- with a `Right` status and a `Board` and updates the Board
    -- with all the proper side effects, this function is a combination
    -- of `sideEffects` and `cleanBoard` and it may be updated to use them,
    -- but the logic seems clearer to me like this.  
    placeNumber :: Square -> Board -> Board
    placeNumber sq@(Square srow scol (Just snono) (Right d)) (Board sqs) = Board (map set sqs)
        where set osq@(Square orow ocol (Just onono) ds) = 
                if ocol == scol && orow == srow then sq
                else if ocol == scol || orow == srow || onono == snono
                    then (Square orow ocol (Just onono) (sub ds))
                else osq
              sub (Left ds) = Left (ds \\ [d])
              sub dd = dd
    
    -- Given an initial `Board` return all the possible solutions starting
    -- from that `Board`. Here, the `[]` monad is used to give an imperative
    -- `feel` to the solution, which seems clearer to me. 
    solveThemAll :: Board -> [Board]
    solveThemAll brd =
        case getLeftSquares (squares brd) of
            [] -> return brd -- Nothing unsolved remains, we are done.
            sqs -> do
                -- Sort the unsolved squares by the ascending length of the possible
                -- digits. Pick the first of those so we always solve forced squares
                -- first.
                let Square c r b (Left ds) : _ = sortBy leftLen sqs
                    leftLen (Square _ _ _ (Left ds1)) (Square _ _ _ (Left ds2)) = compare (length ds1) (length ds2)
                sq <- [ Square c r b (Right d) | d <- ds ] -- Try all possible moves
                solveThemAll (placeNumber sq brd) -- And solve the extended Board.

