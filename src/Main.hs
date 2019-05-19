module Main where
    import qualified Data.List(nub)
    import Data.Maybe
    import Logic
    import Sudoku
    import Printing
    import System.IO
    import System.Environment

    main :: IO ()
    main = do                       
        -- args <- getArgs
        -- contents <- readFile $ "nonos/inputs/" ++ head args
        -- let board = head $ assemble $ tuplesToNonos $ linesToTuples $ head $ chunks 9 (lines contents)
        let board = head $ assemble test
        -- mapM (putStr . boardToString) boards
        -- putStr $ boardToString board
        let solved = head $ solveThemAll board
        putStr $ show $ squares board

        return ()


    test :: [Nonomino]
    test = [  
        Nonomino 1 [(0, (0, 0)), (0, (0, 1)), (0, (0, 2)), (1, (0, 3)), (0, (0, 4)), (0, (1, 1)), (0, (1, 2)), (0, (1, 3)), (0, (2, 2))],
        Nonomino 2 [(2, (0, 0)), (0, (0, 1)), (3, (0, 2)), (0, (0, 3)), (0, (1,-1)), (0, (1, 0)), (0, (1, 1)), (9, (1, 2)), (0, (1, 3))],
        Nonomino 3 [(6, (0, 0)), (0, (1, 0)), (0, (1, 1)), (8, (2, 0)), (0, (2, 1)), (0, (2, 2)), (0, (2, 3)), (0, (1, 3)), (4, (3, 2))],
        Nonomino 4 [(0, (0, 0)), (0, (0, 1)), (0, (0, 2)), (8, (0, 3)), (0, (0, 4)), (0, (1, 3)), (0, (1, 4)), (0, (2, 3)), (0, (2, 4))],
        Nonomino 5 [(7, (0, 0)), (0, (1, 0)), (5, (1,-1)), (0, (2,-1)), (2, (2,-2)), (9, (3,-2)), (1, (4,-2)), (0, (5,-2)), (0, (5,-3))],
        Nonomino 6 [(4, (0, 0)), (0, (0, 1)), (6, (1, 0)), (0, (1, 1)), (0, (2, 0)), (0, (2, 1)), (0, (2,-1)), (0, (3, 1)), (0, (4, 1))],
        Nonomino 7 [(0, (0, 0)), (0, (0, 1)), (0, (1, 0)), (0, (1, 1)), (3, (2, 0)), (0, (2, 1)), (4, (3, 0)), (0, (3, 1)), (0, (4, 0))],
        Nonomino 8 [(0, (0, 0)), (0, (0, 1)), (0, (1, 0)), (0, (1, 1)), (0, (2, 0)), (0, (2, 1)), (7, (3, 0)), (0, (3, 1)), (0, (3,-1))],
        Nonomino 9 [(8, (0, 0)), (0, (0, 1)), (5, (0, 2)), (7, (1, 0)), (0, (1, 1)), (9, (1, 2)), (3, (2, 0)), (6, (2, 1)), (0, (2, 2))]
        ]
    