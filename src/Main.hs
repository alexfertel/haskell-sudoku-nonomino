module Main where
    import qualified Data.List(nub)
    import Data.Maybe
    import Logic
    import Sudoku
    import Printing
    import Test
    import System.IO
    import System.Environment

    main :: IO ()
    main = do                       
        -- args <- getArgs
        -- contents <- readFile $ "nonos/inputs/" ++ head args
        -- let board = head $ assembleThemAll $ tuplesToNonos $ linesToTuples $ head $ chunks 9 (lines contents)
        let board = cleanBoard $ head $ assembleThemAll $ head cases
        let board' = head $ assembleThemAll $ head cases
        -- mapM (putStr . boardToString) boards
        let solved = head $ solveThemAll board
        putStrLn $ boardToString board
        putStrLn $ boardToSudoku board

        -- let board' = updateBoard [Square 8 8 (Just 8) (Right 6), Square 7 8 (Just 8) (Right 6)] board
        -- putStrLn $ boardToSudoku board'

        -- let sqs = sideEffects [Square 8 7 (Just 8) (Right 7)] board
        -- mapM (putStrLn . show) $ sqs

        -- putStrLn ""

        -- let sqs' = sideEffects [Square 8 7 (Just 8) (Right 7)] board'
        -- mapM (putStrLn . show) $ sqs'

        -- mapM (putStrLn . show) $ squares board
        putStrLn $ boardToSudoku solved

        return ()    