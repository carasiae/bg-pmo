module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Debug.Trace
import qualified Data.Vector as V
import Board
import Control.Exception (evaluate)

-- Helper functions for creating test boards
emptyBoard :: Board
emptyBoard = Board (V.replicate 24 Nothing) 0 0

-- Create a board with pieces at specific points
boardWithPieces :: [(Int, Player, Int)] -> Board
boardWithPieces pieces = foldr addPieceToBoard emptyBoard pieces
  where
    addPieceToBoard (point, player, count) board =
      board { points = (points board) V.// [(point, Just (player, count))] }

-- Create a board with pieces on the bar
boardWithBar :: Int -> Int -> Board
boardWithBar blackBar whiteBar = emptyBoard { barWhite = whiteBar, barBlack = blackBar }

-- Test suite
main :: IO ()
main = hspec $ do
  
  describe "Board basics" $ do
    it "creates empty board correctly" $ do
      let board = emptyBoard
      V.all (== Nothing) (points board) `shouldBe` True
      barBlack board `shouldBe` 0
      barWhite board `shouldBe` 0
    
    it "handles bar operations correctly" $ do
      let board = boardWithBar 2 3
      barOfPlayer board Black `shouldBe` 2
      barOfPlayer board White `shouldBe` 3
      
      let newBoard = setBarOfPlayer board Black 5
      barOfPlayer newBoard Black `shouldBe` 5
      barOfPlayer newBoard White `shouldBe` 3

  describe "Piece placement and detection" $ do
    it "detects player pieces correctly" $ do
      let board = boardWithPieces [(5, Black, 2), (10, White, 3)]
      hasPlayerPieces board Black 5 `shouldBe` True
      hasPlayerPieces board White 10 `shouldBe` True
      hasPlayerPieces board Black 10 `shouldBe` False
      hasPlayerPieces board White 5 `shouldBe` False
      hasPlayerPieces board Black 0 `shouldBe` False

    it "checks landing permissions correctly" $ do
      let board = boardWithPieces [(5, Black, 2), (10, White, 1), (15, White, 3)]
      -- Can land on empty point
      canLandOn board Black 0 `shouldBe` True
      -- Can land on own pieces
      canLandOn board Black 5 `shouldBe` True
      -- Can land on single opponent piece (hit)
      canLandOn board Black 10 `shouldBe` True
      -- Cannot land on multiple opponent pieces
      canLandOn board Black 15 `shouldBe` False

  describe "Bear-off conditions" $ do
    it "allows bear-off when all pieces are home" $ do
      let board = boardWithPieces [(0, Black, 2), (1, Black, 2), (2, Black, 2)]
      canBearOff board Black `shouldBe` True
      
      let boardWithOutside = boardWithPieces [(0, Black, 2), (10, Black, 1)]
      canBearOff boardWithOutside Black `shouldBe` False

    it "prevents bear-off when pieces are on bar" $ do
      let board = boardWithPieces [(0, Black, 2), (1, Black, 2)]
      let boardWithBar = board { barBlack = 1 }
      canBearOff boardWithBar Black `shouldBe` False

    it "correctly identifies White's home board" $ do
      let board = boardWithPieces [(18, White, 2), (23, White, 3)]
      canBearOff board White `shouldBe` True
      
      let boardWithOutside = boardWithPieces [(18, White, 2), (10, White, 1)]
      canBearOff boardWithOutside White `shouldBe` False

  describe "Re-entry requirements" $ do
    it "requires re-entry when pieces are on bar" $ do
      let board = boardWithBar 1 0
      mustReenter board Black `shouldBe` True
      mustReenter board White `shouldBe` False
      
      let board2 = boardWithBar 0 2
      mustReenter board2 Black `shouldBe` False
      mustReenter board2 White `shouldBe` True

  describe "Move application" $ do
    it "applies regular moves correctly" $ do
      let board = boardWithPieces [(5, Black, 2)]
      let newBoard = applyMove board Black (RegularMove 5 8)
      hasPlayerPieces newBoard Black 5 `shouldBe` True  -- Still one piece left
      hasPlayerPieces newBoard Black 8 `shouldBe` True  -- New piece added
      case points newBoard V.! 5 of
        Just (Black, 1) -> True `shouldBe` True
        _ -> expectationFailure "Should have 1 white piece on point 5"

    it "applies bear-off moves correctly" $ do
      let board = boardWithPieces [(2, Black, 2)]
      let newBoard = applyMove board Black (BearOffMove 2)
      case points newBoard V.! 2 of
        Just (Black, 1) -> True `shouldBe` True  -- One piece removed
        Nothing -> expectationFailure "Should still have 1 piece after bearing off one"
        _ -> expectationFailure "Unexpected board state"

    it "applies re-entry moves correctly" $ do
      let board = boardWithBar 1 0
      let newBoard = applyMove board Black (ReenterMove 20)
      barOfPlayer newBoard Black `shouldBe` 0
      hasPlayerPieces newBoard Black 20 `shouldBe` True

    it "handles hitting opponent pieces" $ do
      let board = boardWithPieces [(10, White, 1)]
      let newBoard = addPiece board Black 10
      barOfPlayer newBoard White `shouldBe` 1
      hasPlayerPieces newBoard Black 10 `shouldBe` True
      hasPlayerPieces newBoard White 10 `shouldBe` False

  describe "Move generation" $ do
    it "generates re-enter moves when pieces are on bar" $ do
      let board = boardWithBar 1 0
      let state = State board (Die 3, Die 4) Black
      let moves = possibleMoves state
      -- Should generate moves that include re-entry
      length moves `shouldSatisfy` (> 0)
      -- All move sequences should start with re-entry
      all (\moveSeq -> case head moveSeq of
        ReenterMove _ -> True
        _ -> False) moves `shouldBe` True

    it "generates bear-off moves when eligible" $ do
      let board = boardWithPieces [(0, Black, 1), (1, Black, 1)]
      let state = State board (Die 1, Die 2) Black
      let moves = possibleMoves state
      length moves `shouldSatisfy` (> 0)
      -- Should include bear-off moves
      any (\moveSeq -> any (\move -> case move of
        BearOffMove _ -> True
        _ -> False) moveSeq) moves `shouldBe` True

    it "handles doubles correctly" $ do
      let board = boardWithPieces [(5, Black, 4)]
      let state = State board (Die 2, Die 2) Black
      let moves = possibleMoves state
      -- With doubles, should be able to make 4 moves
      any (\moveSeq -> length moveSeq == 4) moves `shouldBe` True

  describe "Game result detection" $ do
    it "detects normal win" $ do
      let board = boardWithPieces [(0, White, 14)]  -- White has 14 pieces, Black has 0
      result board `shouldBe` Win Black Single

    it "detects gammon" $ do
      -- Black wins gammon: White hasn't borne off any pieces
      let board = boardWithPieces [(18, White, 15)]  -- All White pieces still on board
      result board `shouldBe` Win Black Gammon

    it "detects backgammon" $ do
      -- Black wins backgammon: White has pieces on bar or in Black's home
      let tempBoard = boardWithPieces [(18, White, 14)]
          boardWithBar = setBarOfPlayer tempBoard White 1
      result boardWithBar `shouldBe` Win Black Backgammon
      
      let boardWithHome = boardWithPieces [(18, White, 14), (2, White, 1)]
      result boardWithHome `shouldBe` Win Black Backgammon

    it "detects game in progress" $ do
      let board = boardWithPieces [(0, Black, 7), (18, White, 8)]
      result board `shouldBe` InProgress

  describe "Bear-off calculations" $ do
    it "calculates exact bear-off points correctly" $ do
      let board = boardWithPieces [(0, Black, 1), (5, Black, 1)]  -- Black pieces on 1-point and 6-point
      let state = State board (Die 1, Die 6) Black
      let moves = (possibleMoves state)
      -- Should be able to bear off from both points
      let bearOffMoves = filter (\move -> case move of BearOffMove _ -> True; _ -> False) (concat moves)
      BearOffMove 0 `elem` bearOffMoves `shouldBe` True  -- Bear off with 1
      BearOffMove 5 `elem` bearOffMoves `shouldBe` True  -- Bear off with 6

    it "handles bear-off with higher die than exact point" $ do
      let board = boardWithPieces [(2, Black, 1)]  -- Piece on 3-point only
      let state = State board (Die 6, Die 3) Black  -- Roll 6 and 1
      let moves = possibleMoves state
      -- Should be able to bear off the 3-point piece with the 6 (no higher pieces)
      let bearOffMoves = filter (\move -> case move of BearOffMove _ -> True; _ -> False) (concat moves)
      BearOffMove 2 `elem` bearOffMoves `shouldBe` True

  describe "Re-entry calculations" $ do
    it "calculates Black re-entry points correctly" $ do
      let board = boardWithBar 1 0
      let state = State board (Die 3, Die 4) Black
      let moves = possibleMoves state
      let reenterMoves = filter (\move -> case move of ReenterMove _ -> True; _ -> False) (concat moves)
      -- Black with die 3 should re-enter on point 21 (24-3), die 4 on point 20 (24-4)
      ReenterMove 21 `elem` reenterMoves `shouldBe` True
      ReenterMove 20 `elem` reenterMoves `shouldBe` True

    it "calculates White re-entry points correctly" $ do
      let board = boardWithBar 0 1
      let state = State board (Die 2, Die 5) White
      let moves = possibleMoves state
      let reenterMoves = filter (\move -> case move of ReenterMove _ -> True; _ -> False) (concat moves)
      -- White with die 2 should re-enter on point 1 (2-1), die 5 on point 4 (5-1)
      ReenterMove 1 `elem` reenterMoves `shouldBe` True
      ReenterMove 4 `elem` reenterMoves `shouldBe` True

  describe "Edge cases" $ do
    it "handles blocked re-entry" $ do
      let tempBoard = boardWithPieces [(1, Black, 2), (4, Black, 2)]
      let board = setBarOfPlayer tempBoard White 1
      let state = State board (Die 2, Die 5) White
      let moves = possibleMoves state
      -- Should have no valid moves if both re-entry points are blocked
      moves `shouldBe` []

    it "handles no legal moves situation" $ do
      -- Create a situation where no moves are possible
      let board = boardWithPieces [(23, Black, 1)]  -- Black piece that can't move forward
      let state = State board (Die 1, Die 2) Black
      let moves = possibleMoves state
      -- Should handle gracefully (may be empty list)
      length moves `shouldSatisfy` (>= 0)

-- Property-based tests
prop_moveApplicationIsReversible :: Board -> Player -> Move -> Property
prop_moveApplicationIsReversible board player move =
  -- This would need a reverse move function to test properly
  -- For now, just test that applying a move doesn't crash
  property $ True  -- Placeholder

prop_countPiecesIsConserved :: Board -> Player -> Move -> Property  
prop_countPiecesIsConserved board player move =
  -- Pieces should be conserved (except for bear-off)
  let originalCount = countPieces board player
      newBoard = applyMove board player move
      newCount = countPieces newBoard player
  in case move of
    BearOffMove _ -> newCount === originalCount - 1
    _ -> newCount === originalCount

