module Main where

import Minesweeper
import WebFudgets
import HasteExtras(addStyleLink)
import Haste.Graphics.Canvas

c1 :: Cell
c1 = C Mine True
c2 = [C Mine True,C (Nearby 2) True,C (Nearby 0) True,C Mine False]

b1 = Board { rows = [[C Mine True,C (Nearby 1) True,C (Nearby 1) True],
                      [C (Nearby 0) True,C Mine False,C Mine False]]}

boardToCellList :: Board -> [Cell]
boardToCellList b = concat (rows b)

boardWidth :: Board -> Int
boardWidth b = length (head (rows b))

main :: IO ()
main = do addStyleLink "minesweeper.css"
          runF (h1F (textF "Mine sweeper") >+ game >+ contribution)

game = divF (tableF (boardWidth b1) buttonsF)
{-
buttonsF = listF boardToButtons

boardToButtons = [(val, buttonF (cellToButtonStr cell))
  | val <- [1..(length c2)],
    cell <- c2
-}

buttonsF = listF (cellsToButtons (boardToCellList b1))

cellsToButtons []     = []
cellsToButtons (c:cs) = (c, buttonF (cellToButtonStr c)) : cellsToButtons cs

-- [(val, s) | val <- [1..5], s <- ['a'..'b']]

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          False) = " "
cellToButtonStr (C Mine       _)    = "¤"
cellToButtonStr (C (Nearby n) _)    = show n



--(3, textF "3")

{-data CalculatorButton
     = Plus | Minus | Times | Div
     | Enter | Clear | Digit Int
     deriving Eq
buttonsF = listF [d 7, d 8, d 9,op Div,
                  d 4, d 5, d 6,op Times,
                  d 1, d 2, d 3,op Minus,
                  clr,d 0, ent,op Plus]

d n = (Digit n,buttonF (show n))
op o = (o,buttonF (opLabel o))
ent = op Enter
clr = op Clear

opLabel Plus  = "+"
opLabel Minus = "-"
opLabel Times = "×"
opLabel Div   = "÷"
opLabel Enter = "Ent"
opLabel Clear = "C"
-}

contribution = textF "Copyright 2017. By Alfred and Martin."
