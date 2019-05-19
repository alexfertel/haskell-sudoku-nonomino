module Sudoku where

import Data.List

-- data Board = Board {
--     squares :: [Square]
-- } deriving Show

-- data Square = Square {
--     col :: Int,
--     row :: Int,
--     nono :: Nonomino,
--     status :: Either [Int] Int
-- } deriving Show


-- -- Devuelve la lista de las tableros sudokus validos que se pueden generar a partir 
-- -- de la lista de nonominos dada
-- -- buildSudokus :: [Nonomino] -> [[((Int, Int), Nonomino)]]
-- buildSudokus ns = map fromJust $ filter (isJust) [validMatch permutation ns [] | permutation <- permutations]


-- -- Devuelve si la representacion del sudoku que se genera al unir los nonominos de izq a der 
-- -- en el orden dado es valida, si no se devuelve [] 
-- -- validMatch :: [Int] -> [Nonomino] -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
-- validMatch [] _ board = Just board
-- validMatch (h:t) ns board
--     | isNothing newBoard = Nothing
--     | otherwise = validMatch t nonos $ fromJust newBoard
--     where newBoard = placeNonomino (nonos !! h) board

-- -- Devuelve la nueva representacion del sudoku que se genera al colocar el nonomino, 
-- -- en un sudoku dado, si no es posible devuelve [] 
-- -- placeNonomino :: Nonomino -> [((Int, Int), Nonomino)] -> [((Int, Int), Nonomino)]
-- placeNonomino nono list
--     | (overlapTiles /= []) || (not $ isInside (Nonomino tiles) firstEmpty) = []
--     | otherwise = list ++ [(firstEmpty, Nonomino tiles)]
--         where
--             firstEmpty = head $ getEmptyTiles list		-- Primera casilla vacia
--             a = fst firstEmpty
--             b = snd firstEmpty
--             overlapTiles = [ (i+a, j+b) | (i, j, _) <- tiles, isUsed (i+a, j+b) list]
--             isInside (Nonomino tiles) (a, b) = 
--                 let outsideTile = find (\(x,y,_) -> (x+a>8) || (y+b>8) || (y+b<0)) tiles
--                 in outsideTile == Nothing 


-- -- Dado la posicion de una casilla y una representacion del sudoku devuelve si 
-- -- esta casilla esta ocupada en esa representacion 
-- isUsed :: (Int, Int) -> [((Int, Int), Nonomino)] -> Bool
-- isUsed (x, y) list = elem (x, y) $ getUsedTiles list


-- -- Devuelve los pares ordenados (i, j) que estan ocupados por algun Nonomino
-- getUsedTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
-- getUsedTiles list = [ (a+i, b+j) | ((a, b), Nonomino tiles) <- list, (i,j,_) <- tiles]


-- -- Devuelve los pares ordenados (i, j) que esten desocupados
-- getEmptyTiles :: [((Int, Int), Nonomino)] -> [(Int, Int)]
-- getEmptyTiles list = [ (i, j) | i <- [0..8], j <- [0..8], not (elem (i, j) $ getUsedTiles list)]



