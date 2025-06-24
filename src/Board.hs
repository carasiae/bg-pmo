module Board where

import Data.Function
import Data.List
import Data.List.Extra (nubOrd)
import qualified Data.Vector as V
import System.Random

data Board = Board
  { points :: V.Vector (Maybe (Player, Int)),
    barBlack :: Int,
    barWhite :: Int
  }
  deriving (Show, Eq, Ord)

newtype Die = Die Int
  deriving (Show, Eq, Ord)

data State = State
  { board :: Board,
    dice :: (Die, Die),
    player :: Player
  }
  deriving (Show, Eq, Ord)

data Player = White | Black deriving (Show, Eq, Ord)

data Result = Win Player WinType | InProgress deriving (Show, Eq, Ord)

data WinType = Single | Gammon | Backgammon deriving (Show, Eq, Ord)

data Move
  = RegularMove Int Int
  | ReenterMove Int
  | BearOffMove Int
  deriving (Eq, Ord)

instance Show Move where
  show (RegularMove from to) = show (from + 1) ++ " -> " ++ show (to + 1)
  show (ReenterMove to) = "bar -> " ++ show (to + 1)
  show (BearOffMove from) = show (from + 1) ++ " -> off"

barOfPlayer :: Board -> Player -> Int
barOfPlayer b Black = barBlack b
barOfPlayer b White = barWhite b

setBarOfPlayer :: Board -> Player -> Int -> Board
setBarOfPlayer b Black n = b {barBlack = n}
setBarOfPlayer b White n = b {barWhite = n}

rollDice :: IO (Die, Die)
rollDice = do
  n <- (+ 1) . (`mod` 6) <$> randomIO
  m <- (+ 1) . (`mod` 6) <$> randomIO
  if m /= n
    then return (Die n, Die m)
    else rollDice

possibleMoves :: State -> [[Move]]
possibleMoves (State b (Die d1, Die d2) p) =
  filter (\s -> length s == maxLength) sequences
    & sort
    & group
    & map head
  where
    availableDice = if d1 == d2 then [d1, d1, d1, d1] else [d1, d2]
    dicePerms = nubOrd (permutations availableDice)
    sequences = concatMap (\ds -> generateMoveSequences b p ds []) dicePerms
    maxLength = foldr (max . length) 0 sequences

generateMoveSequences :: Board -> Player -> [Int] -> [Move] -> [[Move]]
generateMoveSequences b p remainingDice currentMoves
  | null remainingDice = [reverse currentMoves]
  | otherwise =
      let possibleNextMoves = getValidMovesForDie b p (head remainingDice)
          validSequences = concatMap tryMove possibleNextMoves
       in if null validSequences && null currentMoves
            then []
            else
              if null validSequences
                then [reverse currentMoves]
                else validSequences
  where
    tryMove :: Move -> [[Move]]
    tryMove move =
      case applyMoveIfValid b p move of
        Nothing -> []
        Just newBoard ->
          generateMoveSequences newBoard p (tail remainingDice) (move : currentMoves)

getValidMovesForDie :: Board -> Player -> Int -> [Move]
getValidMovesForDie b p die =
  reenterMoves ++ regularMoves ++ bearOffMoves
  where
    reenterMoves =
      if mustReenter b p
        then case tryReenter die of
          Just move -> [move]
          Nothing -> []
        else []

    regularMoves =
      if not (mustReenter b p)
        then concatMap (movesFromPoint die) [0 .. 23]
        else []

    bearOffMoves =
      if not (mustReenter b p) && canBearOff b p
        then case tryBearOff die of
          Just move -> [move]
          Nothing -> []
        else []

    tryReenter :: Int -> Maybe Move
    tryReenter d =
      let target = if p == Black then 24 - d else d - 1
       in if target >= 0 && target <= 23 && canLandOn b p target
            then Just (ReenterMove target)
            else Nothing

    tryBearOff :: Int -> Maybe Move
    tryBearOff d =
      let exactPoint = if p == Black then d - 1 else 24 - d
          homeRange = if p == Black then [0 .. 5] else [18 .. 23]
       in if exactPoint >= 0 && exactPoint <= 23 && hasPlayerPieces b p exactPoint
            then Just (BearOffMove exactPoint)
            else case findFarthestOccupied b p homeRange of
              Just farthest ->
                let requiredDie = if p == Black then farthest + 1 else 25 - farthest
                 in if d >= requiredDie then Just (BearOffMove farthest) else Nothing
              Nothing -> Nothing

    movesFromPoint :: Int -> Int -> [Move]
    movesFromPoint d point
      | not (hasPlayerPieces b p point) = []
      | otherwise =
          let target = if p == Black then point - d else point + d
           in [RegularMove point target | target >= 0 && target <= 23 && canLandOn b p target]

applyMoveIfValid :: Board -> Player -> Move -> Maybe Board
applyMoveIfValid b p move =
  case move of
    RegularMove from to ->
      if from >= 0
        && from <= 23
        && to >= 0
        && to <= 23
        && hasPlayerPieces b p from
        && canLandOn b p to
        then Just (applyMove b p move)
        else Nothing
    ReenterMove to ->
      if barOfPlayer b p > 0 && to >= 0 && to <= 23 && canLandOn b p to
        then Just (applyMove b p move)
        else Nothing
    BearOffMove from ->
      if canBearOff b p && from >= 0 && from <= 23 && hasPlayerPieces b p from
        then Just (applyMove b p move)
        else Nothing

canLandOn :: Board -> Player -> Int -> Bool
canLandOn b p point
  | point < 0 || point > 23 = False
  | otherwise =
      case points b V.! point of
        Nothing -> True
        Just (p', count) -> p == p' || count == 1

hasPlayerPieces :: Board -> Player -> Int -> Bool
hasPlayerPieces b p point
  | point < 0 || point > 23 = False
  | otherwise =
      case points b V.! point of
        Just (p', _) -> p == p'
        Nothing -> False

findFarthestOccupied :: Board -> Player -> [Int] -> Maybe Int
findFarthestOccupied b p pointRange =
  case filter (hasPlayerPieces b p) pointRange of
    [] -> Nothing
    occupied -> Just (if p == Black then maximum occupied else minimum occupied)

canBearOff :: Board -> Player -> Bool
canBearOff b p =
  barOfPlayer b p == 0
    && not (any (hasPlayerPieces b p) notHome)
  where
    notHome = if p == Black then [6 .. 23] else [0 .. 17]

mustReenter :: Board -> Player -> Bool
mustReenter b p = barOfPlayer b p > 0

result :: Board -> Result
result b
  | minim == 0 && maxim < 15 = Win minPlayer Single
  | minim == 0 && maxim == 15 && not (hasBackgammonCondition b maxPlayer) = Win minPlayer Gammon
  | minim == 0 && maxim == 15 && hasBackgammonCondition b maxPlayer = Win minPlayer Backgammon
  | otherwise = InProgress
  where
    black = countPieces b Black
    white = countPieces b White
    minim = min white black
    maxim = max white black
    minPlayer = if minim == black then Black else White
    maxPlayer = if minPlayer == Black then White else Black

hasBackgammonCondition :: Board -> Player -> Bool
hasBackgammonCondition b p =
  barOfPlayer b p > 0 || any (hasPlayerPieces b p) home
  where
    home = if p == White then [0 .. 5] else [18 .. 23]

countPieces :: Board -> Player -> Int
countPieces b p =
  barOfPlayer b p
    + foldr f 0 (points b)
  where
    f (Just (p', m)) n
      | p' == p = m + n
      | otherwise = n
    f Nothing n = n

applyMove :: Board -> Player -> Move -> Board
applyMove b p move =
  case move of
    RegularMove from to ->
      addPiece (removePiece b p from) p to
    ReenterMove to ->
      let boardAfterReenter = setBarOfPlayer b p (barOfPlayer b p - 1)
          boardAfterAdd = addPiece boardAfterReenter p to
       in boardAfterAdd
    BearOffMove from ->
      removePiece b p from

removePiece :: Board -> Player -> Int -> Board
removePiece b p point
  | point < 0 || point > 23 = b
  | otherwise =
      let newPoints = updatePointAt (points b) point removeFromPoint
       in b {points = newPoints}
  where
    removeFromPoint :: Maybe (Player, Int) -> Maybe (Player, Int)
    removeFromPoint (Just (p', count))
      | p' /= p = Just (p', count)
      | count > 1 = Just (p', count - 1)
      | otherwise = Nothing
    removeFromPoint Nothing = Nothing

addPiece :: Board -> Player -> Int -> Board
addPiece b p point
  | point < 0 || point > 23 = b
  | otherwise =
      let currentPoint = points b V.! point
          (newPoints, newBoard) = case currentPoint of
            Nothing ->
              (updatePointAt (points b) point (const (Just (p, 1))), b)
            Just (p', count)
              | p == p' ->
                  (updatePointAt (points b) point (const (Just (p', count + 1))), b)
              | count == 1 ->
                  let boardWithHit = setBarOfPlayer b p' (barOfPlayer b p' + 1)
                      updatedPoints = updatePointAt (points boardWithHit) point (const (Just (p, 1)))
                   in (updatedPoints, boardWithHit)
              | otherwise ->
                  (points b, b)
       in newBoard {points = newPoints}

updatePointAt :: V.Vector a -> Int -> (a -> a) -> V.Vector a
updatePointAt xs index f
  | index < 0 || index >= V.length xs = xs
  | otherwise = xs V.// [(index, f $ xs V.! index)]

prettyBoard :: Board -> String
prettyBoard b =
  let pts = points b
      -- Top: points 13 to 24 (indices 12 to 23)
      topPoints = map (formatPoint . (pts V.!)) [12 .. 23]
      -- Bottom: points 12 to 1 (indices 11 down to 0)
      bottomPoints = map (formatPoint . (pts V.!)) (reverse [0 .. 11])
      topLabels = centerStrings 5 $ map show ([13 .. 24] :: [Int])
      bottomLabels = centerStrings 5 $ map show (reverse [1 .. 12] :: [Int])
      barInfo = "Bar: W[" ++ show (barWhite b) ++ "] B[" ++ show (barBlack b) ++ "]"
   in unlines
        [ unwords topLabels,
          unwords topPoints,
          replicate ((length topPoints * 6) - 1) '-',
          unwords bottomPoints,
          unwords bottomLabels,
          barInfo
        ]

formatPoint :: Maybe (Player, Int) -> String
formatPoint Nothing = "     "
formatPoint (Just (p, n)) =
  let pl = case p of White -> "W"; Black -> "B"
   in centerString 5 (pl ++ show n)

-- Utility to center a string of max width n
centerString :: Int -> String -> String
centerString n s = replicate lpad ' ' ++ s ++ replicate rpad ' '
  where
    total = n - length s
    lpad = total `div` 2
    rpad = total - lpad

-- Apply centerString to a list
centerStrings :: Int -> [String] -> [String]
centerStrings n = map (centerString n)

initialBoard :: Board
initialBoard =
  Board
    { points =
        V.fromList
          [ Just (White, 2), -- 1st point
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Just (Black, 5), -- 6th point
            Nothing,
            Just (Black, 3), -- 8th point
            Nothing,
            Nothing,
            Nothing,
            Just (White, 5), -- 12th point
            Just (Black, 5), -- 13th point
            Nothing,
            Nothing,
            Nothing,
            Just (White, 3), -- 17th point
            Nothing,
            Just (White, 5), -- 19th point
            Nothing,
            Nothing,
            Nothing,
            Nothing,
            Just (Black, 2) -- 24th point
          ],
      barWhite = 0,
      barBlack = 0
    }
