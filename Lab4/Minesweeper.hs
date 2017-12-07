module Minesweeper where

import Test.QuickCheck
import Util
import Data.Char hiding (isNumber)

-- run "cabal install grid" for this to work, then restart ghci
-- use "haste-cabal install grid" when using haste
import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal

data CellType = Mine | Nearby Int
  deriving (Eq,Show)

data Cell = C CellType Bool
  deriving (Eq,Show)

data Board = Board { rows :: [[Cell]] }
  deriving (Show)


-----------------------------------------------------------------------------
-- * Printing Board

printBoard :: Board -> IO ()
printBoard b = putStrLn (concatMap boardLineToString (rows b))

boardLineToString :: [Cell] -> String
boardLineToString = foldr ((:) . cellToChar) "\n"

cellToChar :: Cell -> Char
cellToChar (C Mine _)       = 'M'
cellToChar (C (Nearby 0) _) = '.'
cellToChar (C (Nearby n) _) = intToDigit n


-----------------------------------------------------------------------------
-- * Generating Board

-- | Given an int between 0-10 this generates a cell where the int represents
-- | the probability of the cell being a mine.
cell :: Int -> Gen Cell
cell n = frequency [ (n, return (C Mine False))
                   , (10-n, return (C (Nearby 0) False))]

-- | Given a number of columns and a number of rows, this function
-- | generates a board without any bombs.
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols =
  Board [[(C (Nearby 0) False) | _ <- [1..cols]] | _ <-[1..rows]]

-- | Given a number of columns, a number of rows and the probability of mines
-- | represented by an int between 0-10 this function gives
-- | a generator for a board.
board :: Int -> Int -> Int -> Gen Board
board rows cols prob = do rows <- vectorOf rows (vectorOf cols (cell prob))
                          let b = setNearbyMarkers (Board rows)
                          return b

-- | Given a board, this function updates the nearby cells of every mine
-- | with the correct numbers.
setNearbyMarkers :: Board -> Board
setNearbyMarkers b = findMinesOnRows (rows b) b

-- | Given a list of rows and a board with the rows this function updates
-- | the neighbours of every mine in the list of rows.
findMinesOnRows :: [[Cell]] -> Board -> Board
findMinesOnRows []     b = b
findMinesOnRows (r:rs) b = findMinesOnRows rs (findMinesOnRow row_num r b)
  where
    row_num = length (rows b) - length (r:rs)

-- | Inserts the given cell at given position in given board
setCellAt :: Int -> Int -> Cell -> Board -> Board
setCellAt row col cell b = Board (rows b !!= (row,row''))
  where
    row'  = (rows b) !! row
    row'' = row' !!= (col,cell)

-- | Returns the cell from given position in given board
getCellAt :: Int -> Int -> Board -> Cell
getCellAt row col b = ((rows b) !! row) !! col

-- | Given a row number, a row and a complete board this function updates
-- | the neighbours of every mine in the current row.
findMinesOnRow :: Int -> [Cell] -> Board -> Board
findMinesOnRow row_num [] b        = b
findMinesOnRow row_num ((C Mine click):cs) b =
  findMinesOnRow row_num cs (
    incrementAtPositions (neighbours grid (row_num,col_num)) b
    )

  where
    col_num = length (rows b !! 1) - length ((C Mine click):cs)
    grid = rectOctGrid (length (rows b)) (length (rows b !! 1))

findMinesOnRow row_num (c:cs) b    = findMinesOnRow row_num cs b

-- | Given a list of positions (row,col) and a board this function increments
-- | the cells at the positions if they are not mines.
incrementAtPositions :: [(Int,Int)] -> Board -> Board
incrementAtPositions [] b = b
incrementAtPositions ((row,col):xs) b =
  incrementAtPositions xs (incrementAtPosition row col b)

-- | Given a position of row number and col number,
-- | this increments the counter in that cell or does nothing if the cell
-- | is a mine.
incrementAtPosition :: Int -> Int -> Board -> Board
incrementAtPosition row col b =
  setCellAt row col (incrementCell (getCellAt row col b)) b

-- | Given a cell, this function increments the counter within.
-- | If it contains a mine, nothing is done and a mine is returned.
incrementCell :: Cell -> Cell
incrementCell (C Mine b) = (C Mine b)
incrementCell (C (Nearby n) b) = (C (Nearby (n+1)) b)

-----------------------------------------------------------------------------
-- * Clicking Board


-- | Given a row numer, a col number and the board this function returns
-- | Just Board if everything went okay, otherwise Nothing is returned
open :: Int -> Int -> Board -> Maybe Board
open row col b | ct == Mine = Nothing
               | ct == Nearby 0 = Just (explodeBoardAt row col b)
               | otherwise =
                 Just (setCellAt row col (clicked (getCellAt row col b)) b)
  where
    ct = cellTypeAt row col b

cellTypeAt :: Int -> Int -> Board -> CellType
cellTypeAt row col b = cellType ((rows b !! row) !! col)

cellType :: Cell -> CellType
cellType (C ct _) = ct

-- | Takes a cell and sets its boolean value to true
clicked :: Cell -> Cell
clicked (C ct _) = (C ct True)

-- TODO
explodeBoardAt :: Int -> Int -> Board -> Board
explodeBoardAt row col b = b
