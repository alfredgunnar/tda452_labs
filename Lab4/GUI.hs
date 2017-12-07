module Main where

import Minesweeper
import WebFudgets
import HasteExtras(addStyleLink)
import Haste.Graphics.Canvas

main :: IO ()
main = do addStyleLink "minesweeper.css"
          runF ()
