module Minesweeper where

import Test.QuickCheck
import Util
import Data.Char hiding (isNumber)
import Data.Maybe (fromJust, isNothing)
import System.Random

-- run "cabal install grid" for this to work, then restart ghci
-- use "haste-cabal install grid" when using haste
import Math.Geometry.Grid
import Math.Geometry.Grid.Octagonal



-----------------------------------------------------------------------------
-- * Cell definition and helper functions
data Cell = C CellType CellState
  deriving (Eq,Show)

data CellType = Mine | Nearby Int
  deriving (Eq,Show)

data CellState = Clicked | Marked | Idle
  deriving (Eq,Show)

cellType :: Cell -> CellType
cellType (C ct _) = ct

-- | Takes a cell and sets its state to clicked
click :: Cell -> Cell
click (C ct _) = C ct Clicked

mark :: Cell -> Cell
mark (C ct _) = C ct Marked

isClicked :: Cell -> Bool
isClicked (C _ cs) = cs == Clicked

cellState :: Cell -> CellState
cellState (C _ cs) = cs

-- | Given a cell, this function increments the counter within.
-- | If it contains a mine, nothing is done and a mine is returned.
incrementCell :: Cell -> Cell
incrementCell (C Mine cs) = (C Mine cs)
incrementCell (C (Nearby n) cs) = (C (Nearby (n+1)) cs)


-----------------------------------------------------------------------------
-- * Board definition and helper functions
data Board = Board { rows :: [[Cell]] }
  deriving (Eq,Show)

-- | Returns the number of columns in given board
width :: Board -> Int
width b = length (rows b !! 0)

-- | Returns the number of rows in given board
height :: Board -> Int
height b = length (rows b)

-- | Inserts the given cell at given position in given board
setCellAt :: Int -> Int -> Cell -> Board -> Board
setCellAt row col cell b = Board (rows b !!= (row,row''))
  where
    row'  = (rows b) !! row
    row'' = row' !!= (col,cell)

-- | Returns the cell from given position in given board
getCellAt :: Int -> Int -> Board -> Cell
getCellAt row col b = ((rows b) !! row) !! col

-- | Given row, col and board this function returns a list of the neighbour
-- | positions.
cellNeighbours :: Int -> Int -> Board -> [(Int,Int)]
cellNeighbours row col b = neighbours grid (row,col)
  where
    grid = rectOctGrid (height b) (width b)

-- | Given row, col and board this function returns the type of the cell
-- | at the given position
cellTypeAt :: Int -> Int -> Board -> CellType
cellTypeAt row col b = cellType ((rows b !! row) !! col)


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
cellToCharClick (C ct Idle) = 'X'
cellToCharClick (C ct Marked) = 'M'
cellToCharClick (C ct Clicked) = cellToChar (C ct Clicked)


-----------------------------------------------------------------------------
-- * Generating Board

-- | Given a number of columns and a number of rows, this function
-- | generates a board without any bombs.
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols =
  Board [[(C (Nearby 0) Idle) | _ <- [1..cols]] | _ <-[1..rows]]

-- | Given an int between 0-100 this generates a cell where the int
-- | represents the probability of the cell being a mine.
rndCell :: Int -> IO Cell
rndCell n = do rnd <- randomRIO (0,100)
               if rnd <= n
                 then return (C Mine Idle)
                 else return (C (Nearby 0) Idle)

-- | Given a number of rows, a number of columns and the probability of mines
-- | represented by an int between 0-100 this function gives
-- | a generator for a board.
rndBoard :: Int -> Int -> Int -> IO Board
rndBoard rows cols prob =
  do rows <- sequence [sequence [(rndCell prob) | _ <- [1..cols]] | _ <- [1..rows]]
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

-- | Given a row number, a row and a complete board this function updates
-- | the neighbours of every mine in the current row.
findMinesOnRow :: Int -> [Cell] -> Board -> Board
findMinesOnRow row_num [] b        = b
findMinesOnRow row_num ((C Mine cst):cs) b =
  findMinesOnRow row_num cs (
      incrementAtPositions (cellNeighbours row_num col_num b) b
    )
  where
    col_num = (width b) - length ((C Mine cst):cs)

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

explodeBoardAt :: Int -> Int -> Board -> Board
explodeBoardAt row col b
  | cellTypeAt row col b == Mine = b
  | (cellTypeAt row col b == Nearby 0 && not (isClicked (getCellAt row col b)))
  = explodeBoardAtPositions nb
    (setCellAt row col (click (getCellAt row col b)) b)
  | otherwise = (setCellAt row col (click (getCellAt row col b)) b)
  where
    nb = cellNeighbours row col b

-- | Helper function for explodeBoardAt
explodeBoardAtPositions :: [(Int,Int)] -> Board -> Board
explodeBoardAtPositions [] b = b
explodeBoardAtPositions ((row,col):ps) b =
  explodeBoardAtPositions ps (explodeBoardAt row col b)

markAt :: Int -> Int -> Board -> Board
markAt row col b = setCellAt row col (mark (getCellAt row col b)) b


-----------------------------------------------------------------------------
-- * Detecting state

hasWon :: Board -> Bool
hasWon b = hasWon' (concat (rows b))

hasWon' :: [Cell] -> Bool
hasWon' [] = True
hasWon' ((C (Nearby n) cst):cs) | cst == Idle || cst == Marked = False
hasWon' ((C Mine Clicked):cs) = False
hasWon' (c:cs) = hasWon' cs
