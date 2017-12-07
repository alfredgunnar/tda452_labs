module Main where

--import Minesweeper
import WebFudgets
import HasteExtras(addStyleLink)
import Haste.Graphics.Canvas


main :: IO ()
main = do addStyleLink "minesweeper.css"
          runF (h1F (textF "Minesweeper") >+ game >+ contribution)

game = divF (tableF 3 buttonsF)
data CalculatorButton
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


contribution = textF "Copyright 2017. By Alfred and Martin."
