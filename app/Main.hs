module Main (main) where

import Board
import Game
import Bot
import Data.Maybe
import Data.List.Extra
import Control.Concurrent.Async
import System.Random (randomIO)
import Control.Monad

depth :: Int
depth = 1

usedHeuristic :: Heuristic
usedHeuristic = combinedHeuristic

main :: IO ()
main = do
    initDice <- rollDice
    let st = State initialBoard initDice Black
    let go currentState =
            do
            moves <- if player currentState == Black
                     then
                         -- putStrLn ( "Dice Roll:" ++ show (dice currentState))
                         -- putStrLn ("greedy bot chose the move :" ++ show mv)
                         getMoveSequenceFromInput currentState
                     else
                         let mv = fromMaybe [] $ chooseBestMove depth usedHeuristic currentState
                         in do
                         putStrLn ( "Dice Roll:" ++ show (dice currentState))
                         putStrLn ("bg bot chose the move :" ++ show mv)
                         return mv
            nextDice <- rollDice
            let nextState = (applyMoveSequence currentState moves) { dice = nextDice }
            let res = result $ board nextState
            case res of
                Win _ _ -> pure res
                _ -> go nextState
    -- results <- mapConcurrently (\i -> do
    --     cdice <- rollDice
    --     res <-go (st { dice = cdice, player = if even i then Black else White})
    --     print i
    --     return res
    --     ) [1..500]
    -- let gr = group . sort $ results
    -- let cnt = map length gr
    res <- go st
    print res
