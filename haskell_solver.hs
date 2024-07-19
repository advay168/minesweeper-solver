{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

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
  deriving (Show, Eq)

type Board = M.Map Pos Cell

data Game = Game Board Int

getBoard (Game board _) = board

data Proof = Root Pos | Expanded (Constraint, Constraint) deriving (Show)

data Constraint
  = Exactly Proof Int [Pos]
  deriving (Show)

type Constraints = [Constraint]

instance Eq Constraint where
  Exactly _ n1 p1 == Exactly _ n2 p2 = n1 == n2 && p1 == p2

exactly proof n poses = Exactly proof n (nub $ sort poses)

data Action = FlagPos Pos | OpenPos Pos | IDK

instance Semigroup Action where
  IDK <> a = a
  a <> _ = a

instance Monoid Action where
  mempty = IDK

solve :: Game -> Action
solve game = tacticA game <> mconcat ([tacticB, tacticC] <*> constraints)
  where
    board = getBoard game
    constraints = generateAll $ initialConstraints board

tacticA :: Game -> Action
tacticA (Game board totalFlags) =
  if flags == totalFlags
    then action
    else IDK
  where
    flags = length . filter (== Flag) . M.elems $ board
    action = case filter ((== Unknown) . snd) $ M.toList board of
      [] -> IDK
      ((pos, _) : _) -> trace ("Opening because of all flagged" ++ show pos) $ OpenPos pos

tacticB :: Constraint -> Action
tacticB (Exactly proof n poses@(pos : _)) | n == length poses = trace ("Mining " ++ show pos ++ " because of " ++ show proof) $ FlagPos pos
tacticB _ = IDK

tacticC :: Constraint -> Action
tacticC (Exactly proof 0 (pos : _)) = trace ("Opening " ++ show pos ++ " because of " ++ show proof) $ OpenPos pos
tacticC _ = IDK

-- xs `takenOutFrom` ys == if xs `isSubsetOf` ys then Just (ys // xs) else Nothing
takenOutFrom :: Ord a => [a] -> [a] -> Maybe [a]
takenOutFrom [] xs = Just xs
takenOutFrom (_ : _) [] = Nothing
takenOutFrom xxs@(x : xs) (y : ys) = case compare x y of
  LT -> Nothing
  GT -> (y :) <$> takenOutFrom xxs ys
  EQ -> takenOutFrom xs ys

expandConstraints :: Constraints -> Constraints
expandConstraints constraints = do
  p1@(Exactly _ n1 c1s) <- constraints
  p2@(Exactly _ n2 c2s) <- constraints
  case c1s `takenOutFrom` c2s of
    Just [] -> []
    Just remaining -> return $ Exactly (Expanded (p1, p2)) (n2 - n1) remaining
    Nothing -> []

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
            else Just $ exactly (Root pos) n' lst
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
