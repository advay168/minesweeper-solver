import Data.Map.Strict qualified as M
import System.IO

type Pos = (Int, Int)

data Cell = Unknown | Flag | Digit Int | UnknownDigit deriving (Show)

newtype Board = Board (M.Map Pos Cell) deriving (Show)

data Action = FlagPos Pos | OpenPos Pos | IDK

parse :: String -> Board
parse = Board . M.map convert . M.fromList . read
  where
    convert '?' = Unknown
    convert 'F' = Flag
    convert '0' = Digit 0
    convert '1' = Digit 1
    convert '2' = Digit 2
    convert '3' = Digit 3
    convert '4' = Digit 4
    convert '5' = Digit 5
    convert '6' = Digit 6
    convert '7' = Digit 7
    convert '8' = Digit 8
    convert '9' = Digit 9
    convert '#' = UnknownDigit

solve board = seq board FlagPos (1, 1)

renderAction (FlagPos pos) = "F " ++ show pos
renderAction (OpenPos pos) = "O " ++ show pos
renderAction IDK = "?"

main = hSetBuffering stdout NoBuffering >> interact solver
  where
    solver = unlines . map (renderAction . solve . parse) . lines
