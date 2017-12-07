module Main where

import Test.QuickCheck

main :: IO ()
main = putStrLn "Hello World"

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

-- | Given a list, and a tuple containing an index in the list and a new
-- | value, update the given list with the new value at the given index.
-- | This function is taken from lab 3.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _                               = []
(!!=) list (n, _)     | n > length list = error "Too big index"
(!!=) (x:xs) (n, val) | n == 0           = val:xs
                      | n > 0            = x:(!!=) xs (n-1, val)
                      | otherwise = error "Too small index"

-- | Given a position of row number and col number,
-- | this increments the counter in that cell or does nothing if the cell
-- | is a mine.
incrementAtPosition :: Int -> Int -> Board -> Board
incrementAtPosition row col =

-- | Given a cell, this function increments the counter within.
-- | If it contains a mine, nothing is done and a mine is returned.
incrementCell :: Cell -> Cell
incrementCell Mine = Mine
incrementCell Nearby n = Nearby (n+1)
