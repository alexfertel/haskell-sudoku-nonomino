module Sudoku where
    import Logic
    import Data.List
    import Data.Maybe

    -- Get the unsolved Squares from a Board.
    getLeftSquares :: Board -> [Square]
    getLeftSquares (Board sqs) = [ sq | sq@(Square _ _ _ (Left _)) <- sqs ]

    -- Place a given Square on a Board and return the new Board.
    -- Illegal setSquare calls will just error out.  The main work here
    -- is to remove the placed digit from the other Squares on the board
    -- that are in the same column, row, or box.
    setSquare :: Square -> Board -> Board
    setSquare sq@(Square scol srow snono (Right d)) (Board sqs) = Board (map set sqs)
        where set osq@(Square col row nono ds) =
                if col == scol && row == srow then sq
                else if col == scol || row == srow || (fromJust nono) == (fromJust snono) then (Square col row nono (sub ds))
                else osq
              sub (Left ds) = Left (ds \\ [d])
              sub (Right d') | d == d' = error "Impossible setSquare"
              sub dd = dd
    setSquare _ _ = error "Bad setSquare"


    -- Given an initial Board return all the possible solutions starting
    -- from that Board.
    -- Note, this all happens in the list monad and makes use of lazy evaluation
    -- to avoid work.  Using the list monad automatically handles all the backtracking
    -- and enumeration of solutions.
    solveThemAll :: Board -> [Board]
    solveThemAll brd =
        case getLeftSquares brd of
            [] -> return brd            -- Nothing unsolved remains, we are done.
            sqs -> do
                -- Sort the unsolved Squares by the ascending length of the possible
                -- digits.  Pick the first of those so we always solve forced Squares
                -- first.
                let Square c r b (Left ds) : _ = sortBy leftLen sqs
                    leftLen (Square _ _ _ (Left ds1)) (Square _ _ _ (Left ds2)) = compare (length ds1) (length ds2)
                sq <- [ Square c r b (Right d) | d <- ds ] -- Try all possible moves
                solveThemAll (setSquare sq brd) -- And solve the extended Board.

