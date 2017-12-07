module Minesweeper where

import Test.QuickCheck
import Util
import Data.Char hiding (isNumber)

data Cell = Mine | Nearby Int
  deriving (Show)

data Board = Board { rows :: [[Cell]] }
  deriving (Show)


-----------------------------------------------------------------------------
-- * Printing Board

printBoard :: Board -> IO ()
printBoard b = putStrLn (concatMap boardLineToString (rows b))

boardLineToString :: [Cell] -> String
boardLineToString = foldr ((:) . cellToChar) "\n"

cellToChar :: Cell -> Char
cellToChar Mine  = 'M'
cellToChar (Nearby 0) = '.'
cellToChar (Nearby n) = intToDigit n


-----------------------------------------------------------------------------
-- * Generating Board

cell :: Gen Cell
cell = frequency [ (1, return Mine)
                 , (9, return (Nearby 0))]

-- | Given a number of columns and a number of rows, this function
-- | generates a board without any bombs.
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board [[Nearby 0 | _ <- [1..cols]] | _ <-[1..rows]]

board5x5 :: Gen Board
board5x5 = do rows <- vectorOf 5 (vectorOf 5 cell)
              return (Board rows)

-- | Given a position of row number and col number,
-- | this increments the counter in that cell or does nothing if the cell
-- | is a mine.
incrementAtPosition :: Int -> Int -> Board -> Board
incrementAtPosition row col b = Board (rows b !!= (row,row''))
  where
    row'  = (rows b) !! row
    cell  = row' !! col
    row'' = row' !!= (col,incrementCell cell)

-- | Given a cell, this function increments the counter within.
-- | If it contains a mine, nothing is done and a mine is returned.
incrementCell :: Cell -> Cell
incrementCell Mine = Mine
incrementCell (Nearby n) = Nearby (n+1)
