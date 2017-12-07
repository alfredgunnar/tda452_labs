module Minesweeper where

import Test.QuickCheck

data Cell = Mine | Nearby Int
  deriving (Show)

data Board = Board { rows :: [[Cell]] }
  deriving (Show)

cell :: Gen Cell
cell = frequency [ (5, return Mine)
                 , (5, return (Nearby 0))]

instance Arbitrary Cell where
  arbitrary = cell

-- | Given a number of columns and a number of rows, this function
-- | generates a board without any bombs.
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board [[Nearby 0 | _ <- [1..cols]] | _ <-[1..rows]]

board5x5 :: Gen Board
board5x5 =

-- | Given a position of row number and col number,
-- | this increments the counter in that cell or does nothing if the cell
-- | is a mine.
incrementAtPosition :: Int -> Int -> Board -> Board
incrementAtPosition row col b =

-- | Given a cell, this function increments the counter within.
-- | If it contains a mine, nothing is done and a mine is returned.
incrementCell :: Cell -> Cell
incrementCell Mine = Mine
incrementCell Nearby n = Nearby (n+1)
