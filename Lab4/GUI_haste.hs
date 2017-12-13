import Haste.DOM
import Haste.Events
import Data.IORef
import Minesweeper
import Data.Maybe (fromJust, isNothing)
import Util

data Interface = Interface
  { iBoard    :: Int -> Int -> Int -> IO Board
  , iOpen     :: Int -> Int -> Board -> Maybe Board
  , iHasWon   :: Board -> Bool
  , iMarkAt   :: Int -> Int -> Board -> Board
  , iWidth :: Board -> Int
  }

getCellElems f b = do parent <- newElem "div"
                      children <- sequence (getCellElems' 0 f (rows b))
                    --setChildren parent children --(getCellElems' (rows b))
                      return children



getCellElems' n f []     = []
getCellElems' n f (r:rs) = children ++ (getCellElems' (n+1) f rs)
  where
    children = mapi (f n) r

updateProperty [] [] = do return ()
updateProperty (btn:btns) (boardCell: bcs) = do setProp btn "value" (cellToButtonStr boardCell)
                                                updateProperty btns bcs


implementation = Interface
 { iBoard = rndBoard,
   iOpen = open,
   iHasWon = hasWon,
   iMarkAt = markAt,
   iWidth = width
 }

main = runGame implementation

runGame i =
   -- Definition of variables
   do hello <- newTextElem "Minesweeper Deluxe Edition"
      header <- newElem "h1"

      rowinput <- newElem "input"
       `with` [attr "id" =: "row"]

      rowlabeltxt <- newTextElem "Row"
      rowlabel <- newElem "label"
       `with` [attr "for" =: "row"]

      colinput <- newElem "input"
       `with` [attr "id" =: "col"]
      collabeltxt <- newTextElem "Col"
      collabel <- newElem "label"
       `with` [attr "for" =: "col"]

      output <- newElem "input"

      button <- newElem "input"
                  `with` [attr "type" =: "button",
                          attr "value" =: "Update"]

      b <- (iBoard i 10 10 1)
      globalBoard <- newIORef b

      let gameDivWidth = show (30 * (iWidth i b)) ++ "px"

      gameDiv <- newElem "div"
                  `with` [style "width" =: gameDivWidth ]

      let onEventForElems event handler []       = do return ()
      let onEventForElems event handler (e:es) = (onEvent e event handler) : onEventForElems event handler es

      let setProp' propid val e = setProp e propid val

      let clickDetect row col evt = do board <- readIORef globalBoard
                                       let b' = iOpen i row col board



                                       if not (isNothing b')
                                         then do gameBtns <- (getChildren gameDiv)

                                                 let b'' = (fromJust b')
                                                 let boardCells = concat (rows b'')



                                                 updateProperty gameBtns boardCells
                                                 --setProp output "value" (show (mouseButton evt))

                                                 writeIORef globalBoard (fromJust b')

                                                 if (iHasWon i (fromJust b'))
                                                   then do e <- newTextElem "WINNER!"
                                                           appendChild gameDiv e
                                                   else return ()

                                         else do e <- newTextElem "LOSER"
                                                 appendChild gameDiv e


      let newCellElem row col c = do e <- newElem "input"
                                       `with` [attr "type" =: "button",
                                             attr "value" =: cellToButtonStr c,
                                             attr "row" =: show row,
                                             attr "col" =: show col,
                                             style "width" =: "30px",
                                             style "height" =: "30px",
                                             style "background-color" =: "lightyellow" ]
                                     onEvent e Click (clickDetect row col)
                                     return e


      gameBoard <- getCellElems newCellElem b

      appendChild header hello
      appendChild documentBody header

      appendChild documentBody gameDiv
      setChildren gameDiv gameBoard
      appendChild rowlabel rowlabeltxt
      appendChild collabel collabeltxt
      appendChild documentBody rowlabel
      appendChild documentBody rowinput
      appendChild documentBody collabel
      appendChild documentBody colinput
      appendChild documentBody button
      appendChild documentBody output

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)   = " "
cellToButtonStr (C _          Marked) = "M"
cellToButtonStr (C Mine       _)      = "¤"
cellToButtonStr (C (Nearby n) _)      = show n
