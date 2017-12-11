module Minesweeper where

import Test.QuickCheck
import Util
import Data.Char hiding (isNumber)
import Data.Maybe (fromJust, isNothing)

-- run "cabal install grid" for this to work, then restart ghci
-- use "haste-cabal install grid" when using haste
import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal

data CellType = Mine | Nearby Int
  deriving (Eq,Show)

data Cell = C CellType Bool
  deriving (Eq,Show)

data Board = Board { rows :: [[Cell]] }
  deriving (Eq,Show)


-----------------------------------------------------------------------------
-- * Printing Board

-- | Takes a function that converts a list of cells to a string and a board
printBoardWrapper :: ([Cell] -> String) -> Board -> IO ()
printBoardWrapper f b = putStrLn (concatMap f (rows b))

printBoard :: Board -> IO ()
printBoard b = printBoardWrapper boardLineToString b

-- | Prints board and takes into account if cells are clicked or not
printBoardClick :: Board -> IO ()
printBoardClick b = printBoardWrapper boardLineToStringClick b

boardLineToString :: [Cell] -> String
boardLineToString = foldr ((:) . cellToChar) "\n"

boardLineToStringClick :: [Cell] -> String
boardLineToStringClick = foldr ((:) . cellToCharClick) "\n"

cellToChar :: Cell -> Char
cellToChar (C Mine _)       = '¤'
cellToChar (C (Nearby 0) _) = '.'
cellToChar (C (Nearby n) _) = intToDigit n

-- | Converts a cell to a char and takes into account if it is clicked or not
cellToCharClick :: Cell -> Char
cellToCharClick (C ct clicked) | not clicked = 'X'
                               | otherwise = cellToChar (C ct clicked)

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
      incrementAtPositions (cellNeighbours row_num col_num b) b
    )
  where
    col_num = length (rows b !! 1) - length ((C Mine click):cs)

findMinesOnRow row_num (c:cs) b    = findMinesOnRow row_num cs b

-- | Given row, col and board this function returns a list of the neighbour
-- | positions.
cellNeighbours :: Int -> Int -> Board -> [(Int,Int)]
cellNeighbours row col b = neighbours grid (row,col)
  where
    grid = rectOctGrid (length (rows b)) (length (rows b !! 1))

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
                 Just (setCellAt row col (click (getCellAt row col b)) b)
  where
    ct = cellTypeAt row col b

cellTypeAt :: Int -> Int -> Board -> CellType
cellTypeAt row col b = cellType ((rows b !! row) !! col)

cellType :: Cell -> CellType
cellType (C ct _) = ct

-- | Takes a cell and sets its boolean value to true
click :: Cell -> Cell
click (C ct _) = (C ct True)

isClicked :: Cell -> Bool
isClicked (C _ b) = b


explodeBoardAt :: Int -> Int -> Board -> Board
explodeBoardAt row col b
  | cellTypeAt row col b == Mine = b
  | (cellTypeAt row col b == Nearby 0 && not (isClicked (getCellAt row col b)))
  = explodeBoardAtPositions nb
    (setCellAt row col (click (getCellAt row col b)) b)
  | otherwise = (setCellAt row col (click (getCellAt row col b)) b)
  where
    nb = cellNeighbours row col b

explodeBoardAtPositions :: [(Int,Int)] -> Board -> Board
explodeBoardAtPositions [] b = b
explodeBoardAtPositions ((row,col):ps) b =
  explodeBoardAtPositions ps (explodeBoardAt row col b)
