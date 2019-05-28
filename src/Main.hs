module Main where
    import qualified Data.List(nub)
    import Data.Maybe
    import Nonomino
    import Sudoku
    import Printing
    import Test
    import System.IO
    import System.Environment

    main :: IO ()
    main = do                       
        let board = cleanBoard $ head $ assembleThemAll $ head cases
        let solved = head $ solveThemAll board
        putStrLn $ boardToString board
        putStrLn $ boardToSudoku board
        putStrLn $ boardToSudoku solved
        return ()    