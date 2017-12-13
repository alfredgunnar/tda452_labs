import Haste.DOM
import Haste.Events
import Data.IORef
import Minesweeper
import Data.Maybe (fromJust, isNothing)

data Interface = Interface
  { iBoard    :: Int -> Int -> Int -> IO Board
  , iOpen     :: Int -> Int -> Board -> Maybe Board
  , iHasWon   :: Board -> Bool
  , iMarkAt   :: Int -> Int -> Board -> Board
  , iWidth :: Board -> Int
  }

getCellElems f b = do parent <- newElem "div"
                      children <- sequence (getCellElems' f (rows b))
                    --setChildren parent children --(getCellElems' (rows b))
                      return children



getCellElems' f []     = []
getCellElems' f (r:rs) = children ++ (getCellElems' f rs)
  where
    children = map f r

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

      let clickDetect _ = do board <- readIORef globalBoard
                             let b' = iOpen i 0 0 board



                             if not (isNothing b')
                               then do --cells <- getCellElems newCellElem (fromJust b')
                                       --onEventForElems Click clickDetect cells
                                       --gameBoard <- newElem "div"
                                       --setChildren gameBoard cells

                                       gameBtns <- (getChildren gameDiv)

                                       let b'' = (fromJust b')
                                       let boardCells = concat (rows b'')



                                       updateProperty gameBtns boardCells
                                       setProp output "value" "click!"

                                       writeIORef globalBoard (fromJust b')
                                       --appendChild gameDiv gameBoard

                      --                 if (iHasWon i (fromJust b'))
                      --                   then do e <- newTextElem "WINNER!"
                      --                           appendChild gameDiv e
                      --                   else return ()

                               else do e <- newTextElem "LOSER"
                                       appendChild gameDiv e


      let newCellElem c = do e <- newElem "input"
                               `with` [attr "type" =: "button",
                                     attr "value" =: cellToButtonStr c,
                                     style "width" =: "30px",
                                     style "height" =: "30px",
                                     style "background-color" =: "lightyellow" ]
                             onEvent e Click clickDetect
                             return e

      -- Definition of functions
    --  let reloadBoard r c b = do clearChildren gameDiv
    --                             let b' = iOpen i r c b

    --                             if not (isNothing b')
    --                               then do gameBoard <- getCellElems (fromJust b')
    --                                       writeIORef globalBoard (fromJust b')
    --                                       appendChild gameDiv gameBoard

    --                                       if (iHasWon i (fromJust b'))
    --                                         then do e <- newTextElem "WINNER!"
    --                                                 appendChild gameDiv e
    --                                         else return ()

    --                               else do e <- newTextElem "LOSER"
    --                                       appendChild gameDiv e

    --  let setBoardFlag r c b = do clearChildren gameDiv
    --                              let b' = iMarkAt i r c b
    --                              gameBoard <- getCellElems b'
    --                              writeIORef globalBoard b'
    --                              appendChild gameDiv gameBoard

    --  let update mouseData = do row <- getProp rowinput "value"
    --                            col <- getProp colinput "value"
    --                            let r = read row :: Int -- can fail!
    --                            let c = read col :: Int -- can fail!

    --                            board <- readIORef globalBoard
    --                            reloadBoard r c board

    --                            setProp output "value" (show (mouseButton mouseData) ++ "(" ++ show r ++ "," ++ show c ++ ")")

    --  let setFlag _ = do row <- getProp rowinput "value"
    --                     col <- getProp colinput "value"
    --                     let r = read row :: Int -- can fail!
    --                     let c = read col :: Int -- can fail!

    --                     board <- readIORef globalBoard
    --                     setBoardFlag r c board

    --                     setProp output "value" (show "mark" ++ "(" ++ show r ++ "," ++ show c ++ ")")


      gameBoard <- getCellElems newCellElem b
      --a <- onEventForElems Click clickDetect cells

      --gameBoard <- newElem "div"
      --setChildren gameBoard cells



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

    --  onEvent button Click update
    --  onEvent button Wheel setFlag

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)   = " "
cellToButtonStr (C _          Marked) = "M"
cellToButtonStr (C Mine       _)      = "¤"
cellToButtonStr (C (Nearby n) _)      = show n
