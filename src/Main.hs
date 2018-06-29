module Main where
import Picosat
import Data.Char
import Data.List.Split
import Control.Monad
import Data.Maybe
{-

  Matrix is represented as a 9 x 9 grid

  | (1,1) | (1,2) | ... | (1, 9) |
  | (2,1) | ...
  | ...   |
  ---------

  Each cell is given a variable number equivalent to concatenating digits of it's coordinates
  ie: (1,1) == 11

  additionally for each cell variable we create 9 more variables representing each possible assignment to the cells
-}

data Var = Blank | Defined Int

type Row = [Var]
type Grid = [Row]

readGrid :: String -> Maybe Grid
readGrid s
  | length s == 81 = traverse (traverse readCell) . Data.List.Split.chunksOf 9 $ s
  | otherwise      = Nothing
  where
    readCell '.' = Just Blank
    readCell c
      | Data.Char.isDigit c && c > '0' = Just . Defined . Data.Char.digitToInt $ c
      | otherwise = Nothing

showGrid :: Grid -> String
showGrid = unlines . map (unwords . map showCell)
  where
    showCell (Defined x) = show x
    showCell _ = "."

definedness :: Int -> [[Int]]
definedness var = (map (cellVar var) [1..9])
  : [ [-1 * (cellVar var d), -1 * (cellVar var d') ] | d <- [1..9], d' <- [1..9], d < d' ]

validity :: [Int] -> [[Int]]
validity vars = map (\d -> map (`cellVar` d) vars) [1..9]

sudoku :: [[Int]]
sudoku = concat $
  [ definedness (cellVar row col) | row <- [1..9], col <- [1..9] ] ++
  [ validity [cellVar row col | row <- [1..9] ] | col <- [1..9] ] ++
  [ validity [cellVar row col | col <- [1..9] ] | row <- [1..9] ] ++
  [ validity [cellVar (row + x) (col + y) | x <- [0..2], y <- [0..2] ] | row <- [1, 4, 7], col <- [1, 4, 7] ]

cellVar row col = row * 10 + col

fromSolver vars = map (map Defined) $ Data.List.Split.chunksOf 9 values
  where values = map (`mod` 10) vars

main :: IO ()
main = do
  inputs <- lines <$> getContents
  forM_ inputs $ \input ->
    case readGrid input of
      Nothing   -> putStrLn "Invalid input"
      Just grid -> do

        let defs = concat $ zipWith (\rx row -> catMaybes $ zipWith (\cx col -> case col of
                    Blank -> Nothing
                    Defined i -> Just [rx * 100 + cx * 10 + i]
                  ) [1..] row) [1..] grid

        solution <- solve (defs ++ sudoku)

        case solution of
          Unsatisfiable -> print "No solution!"
          Unknown       -> print "No solution found.  "
          Solution vals -> do
            let sols = filter (>0) vals
            let grid = fromSolver sols
            putStrLn (showGrid grid)


