{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Debug.Trace
import System.IO

type Pos = (Int, Int)

data Cell
  = Unknown
  | Flag
  | Digit Int
  | UnknownDigit
  deriving (Eq)

type Board = M.Map Pos Cell

data Game = Game Board Int

getBoard (Game board _) = board

data Proof
  = TotalFlags
  | Root Pos
  | Expanded (Constraint, Constraint)
  | Dexpanded (Constraint, Constraint)
  deriving (Show)

data Constraint
  = Exactly Proof Int [Pos]
  deriving (Show)

type Constraints = [Constraint]

instance Eq Constraint where
  Exactly _ n1 p1 == Exactly _ n2 p2 = n1 == n2 && p1 == p2

data Action = FlagPos Pos | OpenPos Pos | IDK

instance Semigroup Action where
  IDK <> a = a
  a <> _ = a

instance Monoid Action where
  mempty = IDK

solve :: Game -> Action
solve (Game board totalFlags) = mconcat (totalFlagsConstraint : constraints <**> [tacticMine, tacticOpen])
  where
    totalFlagsConstraint = Exactly TotalFlags unFlagged flaggable
    (unFlagged, flaggable) =
      M.foldlWithKey'
        ( \(count, poses) pos cell -> case cell of
            Unknown -> (count, pos : poses)
            Flag -> (count - 1, poses)
            _ -> (count, poses)
        )
        (totalFlags, [])
        board
    constraints = generateAll $ initialConstraints board

tacticMine :: Constraint -> Action
tacticMine (Exactly proof n poses@(pos : _)) | n == length poses = trace ("Mining " ++ show pos ++ " because of " ++ show proof) $ FlagPos pos
tacticMine _ = IDK

tacticOpen :: Constraint -> Action
tacticOpen (Exactly proof 0 (pos : _)) = trace ("Opening " ++ show pos ++ " because of " ++ show proof) $ OpenPos pos
tacticOpen _ = IDK

expandConstraints :: Constraints -> Constraints
expandConstraints constraints = do
  p1@(Exactly _ n1 p1s) <- constraints
  p2@(Exactly _ n2 p2s) <- constraints
  case (p1s \\ p2s, p2s \\ p1s) of
    ([], []) -> assert (n2 - n1 == 0) []
    ([], remaining) -> return $ Exactly (Expanded (p1, p2)) (n2 - n1) remaining
    (_, remaining)
      | n2 - n1 == length remaining -> return $ Exactly (Dexpanded (p1, p2)) (n2 - n1) remaining -- Guaranteed to trigger tactic
    _ -> []

generateAll :: Constraints -> Constraints
generateAll constraints = constraints ++ (if null next then [] else generateAll (constraints ++ next))
  where
    next = expandConstraints (nub constraints) \\ constraints

initialConstraints :: Board -> Constraints
initialConstraints board = mapMaybe generate $ M.toList board
  where
    generate (pos, Digit n) =
      let (n', lst) = foldl' count (n, []) $ neighboursWithCells board pos
          count (n, lst) (_, Flag) = (n - 1, lst)
          count (n, lst) (pos, Unknown) = (n, pos : lst)
          count acc _ = acc
       in if null lst
            then Nothing
            else Just $ Exactly (Root pos) n' lst
    generate (pos, _) = Nothing

neighboursWithCells :: Board -> Pos -> [(Pos, Cell)]
neighboursWithCells board (x, y) = catMaybes $ do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  guard $ dx /= 0 || dy /= 0
  let pos = (x + dx, y + dy)
  return $ (pos,) <$> M.lookup pos board

parse :: String -> Game
parse str = Game board (read totalFlags)
  where
    (totalFlags, remainingStr) = break isSpace str
    board = M.map convert . M.fromList . read $ remainingStr
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
    convert c = error $ "Deserialise Error: Did not expect" ++ [c]

renderAction (FlagPos pos) = "F " ++ show pos
renderAction (OpenPos pos) = "O " ++ show pos
renderAction IDK = "?"

main = hSetBuffering stdout NoBuffering >> interact solver
  where
    solver = unlines . map (renderAction . solve . parse) . lines
