module Game where

import Board
import Text.Read (readMaybe)

getMoveSequenceFromInput :: State -> IO [Move]
getMoveSequenceFromInput state = do
  putStrLn . prettyBoard $ board state
  putStrLn "Your dice: "
  print $ dice state
  putStrLn "Your color: "
  print $ player state
  let sequences = possibleMoves state
  if null sequences
    then putStrLn "No legal moves available." >> return []
    else do
      putStrLn "Available move sequences:"
      mapM_ (\(i, s) -> putStrLn $ show (i + 1 :: Int) ++ ": " ++ show s) (zip [0 ..] sequences)
      promptChoice sequences
  where
    promptChoice :: [[Move]] -> IO [Move]
    promptChoice sequences = do
      putStrLn "Choose sequence number (or '.' to skip):"
      input <- getLine
      case input of
        "." -> return []
        str -> case readMaybe str :: Maybe Int of
          Just i | i > 0 && i <= length sequences -> return (sequences !! (i - 1))
          _ -> putStrLn "Invalid input. Try again." >> promptChoice sequences

applyMoveSequence :: State -> [Move] -> State
applyMoveSequence s ms = (foldl applySingle s ms) {player = if player s == White then Black else White}
  where
    applySingle st move =
      case applyMoveIfValid (board st) (player st) move of
        Just newBoard -> st {board = newBoard}
        Nothing -> st
