module Printing where
    import Logic
    import Data.List

    order :: [Point]
    order = [(x, y) | x <- [0..8], y <- [0..8]]

    colors :: [Int]
    colors = [31..36] ++ [91..95]

    boardToStringWithColors :: Board -> String
    boardToStringWithColors (Board sqs) = unlines [(irow i) ++ "\n" | i <- [0..8]]
        where irow i = concat [squareNonoToString sq | sq <- sort $ filter (\sq -> (row sq) == i) sqs]

    boardToString :: Board -> String
    boardToString (Board sqs) = unlines [irow i | i <- [0..8]]
        where irow i = concat $ intersperse " " [squareNonoToString sq | sq <- sort $ filter (\sq -> (row sq) == i) sqs]

    squareNonoToString :: Square -> String
    squareNonoToString (Square _ _ Nothing _) = "_"
    squareNonoToString (Square _ _ (Just int) _) = show int

    squareValueToString :: Square -> String
    squareValueToString (Square _ _ _ (Left _)) = "_"
    squareValueToString (Square _ _ _ (Right int)) = show int

    squareToStringWithColors :: Square -> String
    squareToStringWithColors (Square _ _ Nothing _) = color 37 "_"
    squareToStringWithColors (Square _ _ (Just int) (Left values)) = color (colors !! int) "*"
    squareToStringWithColors (Square _ _ (Just int) (Right value)) = color (colors !! int) (show value)

    boardToSudoku :: Board -> String
    boardToSudoku (Board sqs) = unlines [irow i | i <- [0..8]]
        where irow i = concat $ intersperse " " [squareValueToString sq | sq <- sort $ filter (\sq -> (row sq) == i) sqs]

    color :: Int -> String -> String
    color c s = "\x1b[" ++ (show c) ++ s ++ "\x1b[0m"
