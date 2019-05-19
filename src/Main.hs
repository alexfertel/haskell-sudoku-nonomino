module Main where
    import qualified Data.List(nub)
    import Data.Maybe
    import Logic
    import Printing
    import System.IO
    import System.Environment

    main :: IO ()
    main = do 
        let dirx = [0, -1, -1, 0, 1, 1, 1, 0, -1]
            diry = [0, 0, 1, 1, 1, 0, -1, -1, -1]
            box (x, y) = [(i + x, j + y) | (i, j) <- zip dirx diry]
            nonos = [ Nonomino 1 (box (1, 1)), 
                      Nonomino 2 (box (1, 4)), 
                      Nonomino 3 (box (1, 7)), 
                      Nonomino 4 (box (4, 1)), 
                      Nonomino 5 (box (4, 4)), 
                      Nonomino 6 (box (4, 7)), 
                      Nonomino 7 (box (7, 1)), 
                      Nonomino 8 (box (7, 4)), 
                      Nonomino 9 (box (7, 7)) ]
                      
        args <- getArgs
        contents <- readFile $ "nonos/inputs" ++ head args
        let board = head $ assemble $ tuplesToNonos $ linesToTuples $ head $ chunks 9 (lines contents)
        -- print contents
        -- let ls = chunks 9 (lines contents)
        -- print ls
        -- let tuples = linesToTuples $ head ls
        -- print tuples
        -- let nonominos = tuplesToNonos tuples
        -- print nonominos
        -- let board = head $ assemble nonominos
        putStr $ boardToString board

        
        -- let board = head $ assemble nonos
        return ()