module Bot where

import Board
import Data.List.Extra
import qualified Data.Vector as V
import Debug.Trace
import Game (applyMoveSequence)

newtype Heuristic = Heuristic (State -> Double)

diceRollsWithProbs :: [((Die, Die), Double)]
diceRollsWithProbs =
  [((Die a, Die b), if a /= b then 2 / 36 else 1 / 36) | a <- [1 .. 6], b <- [a .. 6]]

opponent :: Player -> Player
opponent White = Black
opponent Black = White

data Bounds = Bounds {alpha :: Double, beta :: Double} deriving (Eq, Ord, Show)

chooseBestMove :: Int -> Heuristic -> State -> Maybe [Move]
chooseBestMove depth heuristic state =
  case possibleMoves state of
    [] -> Nothing
    moves ->
      let initialBounds = Bounds (-1 / 0) (1 / 0) -- -infinity, +infinity
          scoredMoves =
            map
              (\m -> (m, expectiminimaxAB depth (applyMoveSequence state m) heuristic initialBounds))
              moves
          best =
            if player state == Black
              then maximumBy (\(_, a) (_, b) -> compare a b) scoredMoves
              else minimumBy (\(_, a) (_, b) -> compare a b) scoredMoves
       in Just (fst best)

expectiminimaxAB :: Int -> State -> Heuristic -> Bounds -> Double
expectiminimaxAB 0 state (Heuristic heuristic) _ = heuristic state
expectiminimaxAB depth state@(State _ _ p) (Heuristic heuristic) bounds
  | not (null (possibleMoves state)) =
      if p == Black
        then maximizingPlayer depth state (Heuristic heuristic) bounds
        else minimizingPlayer depth state (Heuristic heuristic) bounds
  | otherwise = heuristic state

maximizingPlayer :: Int -> State -> Heuristic -> Bounds -> Double
maximizingPlayer depth state heuristic initialBounds =
  fst $
    foldl'
      ( \(maxVal, bounds) move ->
          if maxVal >= beta bounds
            then (maxVal, bounds) -- Pruned
            else
              let value =
                    chanceValueAB
                      depth
                      (board $ applyMoveSequence state move)
                      (opponent $ player state)
                      heuristic
                      bounds
                  newMaxVal = max maxVal value
                  newBounds = bounds {alpha = max (alpha bounds) newMaxVal}
               in (newMaxVal, newBounds)
      )
      (alpha initialBounds, initialBounds)
      (possibleMoves state)

minimizingPlayer :: Int -> State -> Heuristic -> Bounds -> Double
minimizingPlayer depth state heuristic initialBounds =
  fst $
    foldl'
      ( \(minVal, bounds) move ->
          if minVal <= alpha bounds
            then (minVal, bounds) -- Pruned
            else
              let value =
                    chanceValueAB
                      depth
                      (board $ applyMoveSequence state move)
                      (opponent $ player state)
                      heuristic
                      bounds
                  newMinVal = min minVal value
                  newBounds = bounds {beta = min (beta bounds) newMinVal}
               in (newMinVal, newBounds)
      )
      (beta initialBounds, initialBounds)
      (possibleMoves state)

chanceValueAB :: Int -> Board -> Player -> Heuristic -> Bounds -> Double
chanceValueAB depth b p heuristic bounds =
  sum
    [ prob * expectiminimaxAB (depth - 1) (State b (d1, d2) p) heuristic bounds
      | ((d1, d2), prob) <- diceRollsWithProbs
    ]

makeHeuristic :: (State -> Double) -> Heuristic
makeHeuristic heuristic = Heuristic f
  where
    f s =
      case result (board s) of
        Win Black _ -> 1000
        Win White _ -> -1000
        InProgress -> heuristic s

pipCountHeuristic :: Heuristic
pipCountHeuristic = makeHeuristic pipCount

blotScoreHeuristic :: Heuristic
blotScoreHeuristic = makeHeuristic blotScore

blockadeScoreHeuristic :: Heuristic
blockadeScoreHeuristic = makeHeuristic blockadeScore

combinedHeuristic :: Heuristic
combinedHeuristic = makeHeuristic (\s -> pipCount s + blotScore s + blockadeScore s)

pipCount :: State -> Double
pipCount (State b _ _) = fromIntegral (pipCountFor b White) - fromIntegral (pipCountFor b (opponent Black))

pipCountFor :: Board -> Player -> Int
pipCountFor b p =
  barOfPlayer b p * 25 + sum (V.imap pointPip (points b))
  where
    pointPip :: Int -> Maybe (Player, Int) -> Int
    pointPip idx (Just (p', n)) | p == p' = n * dist idx
    pointPip _ _ = 0

    dist :: Int -> Int
    dist idx = if p == Black then idx + 1 else 24 - idx

blotScore :: State -> Double
blotScore (State b _ _) = fromIntegral (numBlots b White - numBlots b Black)
  where
    numBlots b' p' = V.length $ V.filter (isBlot p') (points b')
    isBlot p (Just (p', 1)) = p' == p
    isBlot _ _ = False

homeScore :: State -> Double
homeScore (State b _ _) = fromIntegral (homeBoardStrength b Black - homeBoardStrength b White)
  where
    homeBoardStrength :: Board -> Player -> Int
    homeBoardStrength b' p' = length $ filter (hasPlayerPieces b' p') $ homePoints p'
    homePoints Black = [0 .. 5]
    homePoints White = [18 .. 23]

blockadeScore :: State -> Double
blockadeScore (State b _ _) =
  fromIntegral (blockadeStrength b Black - blockadeStrength b White)

blockadeStrength :: Board -> Player -> Int
blockadeStrength b p = maximum $ 0 : map length (blockedRuns p)
  where
    direction = if p == Black then id else reverse
    playerPoints = direction (V.toList (points b))

    blockedRuns :: Player -> [[Int]]
    blockedRuns pl =
      let indexed = zip [0 ..] playerPoints
          blocked val = case val of
            Just (owner, n) -> owner == pl && n >= 2
            _ -> False
       in groupConsecutives [i | (i, v) <- indexed, blocked v]

groupConsecutives :: [Int] -> [[Int]]
groupConsecutives [] = []
groupConsecutives (x : xs) = go [x] xs
  where
    go [] l = [l]
    go acc [] = [acc]
    go acc@(a : _) (y : ys)
      | y == a + 1 = go (y : acc) ys
      | otherwise = acc : go [y] ys
