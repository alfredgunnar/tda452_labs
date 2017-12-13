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
  }




newBoardElem' f []     = []
newBoardElem' f (r:rs) = children ++ [newElem "br"] ++ (newBoardElem' f rs)
  where
    children = map f r




implementation = Interface
 { iBoard = rndBoard,
   iOpen = open,
   iHasWon = hasWon,
   iMarkAt = markAt
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

      gameDiv <- newElem "div"

      let clickDetect _ = do setProp output "value" "click!"

      let newCellElem c = do e <- newElem "input"
                              `with` [attr "type" =: "button",
                                    attr "value" =: cellToButtonStr c,
                                    style "width" =: "30px",
                                    style "height" =: "30px",
                                    --style "background-color" =: "yellow",
                                    --style "background-color" =: "yellow",
                                    style "background-color" =: "lightyellow" ]
                             onEvent e Click clickDetect
                             return e

      let newBoardElem b = do parent <- newElem "div"
                              children <- sequence (newBoardElem' newCellElem (rows b))
                              setChildren parent children --(newBoardElem' (rows b))
                              return parent


      -- Definition of functions
      let reloadBoard r c b = do clearChildren gameDiv
                                 let b' = iOpen i r c b

                                 if not (isNothing b')
                                   then do gameBoard <- newBoardElem (fromJust b')
                                           writeIORef globalBoard (fromJust b')
                                           appendChild gameDiv gameBoard

                                           if (iHasWon i (fromJust b'))
                                             then do e <- newTextElem "WINNER!"
                                                     appendChild gameDiv e
                                             else return ()

                                   else do e <- newTextElem "LOSER"
                                           appendChild gameDiv e

      let setBoardFlag r c b = do clearChildren gameDiv
                                  let b' = iMarkAt i r c b
                                  gameBoard <- newBoardElem b'
                                  writeIORef globalBoard b'
                                  appendChild gameDiv gameBoard

      let update mouseData = do row <- getProp rowinput "value"
                                col <- getProp colinput "value"
                                let r = read row :: Int -- can fail!
                                let c = read col :: Int -- can fail!

                                board <- readIORef globalBoard
                                reloadBoard r c board

                                setProp output "value" (show (mouseButton mouseData) ++ "(" ++ show r ++ "," ++ show c ++ ")")

      let setFlag _ = do row <- getProp rowinput "value"
                         col <- getProp colinput "value"
                         let r = read row :: Int -- can fail!
                         let c = read col :: Int -- can fail!

                         board <- readIORef globalBoard
                         setBoardFlag r c board

                         setProp output "value" (show "mark" ++ "(" ++ show r ++ "," ++ show c ++ ")")


      gameBoard <- newBoardElem b


      appendChild header hello
      appendChild documentBody header

      appendChild documentBody gameDiv
      appendChild gameDiv gameBoard
      appendChild rowlabel rowlabeltxt
      appendChild collabel collabeltxt
      appendChild documentBody rowlabel
      appendChild documentBody rowinput
      appendChild documentBody collabel
      appendChild documentBody colinput
      appendChild documentBody button
      appendChild documentBody output

      onEvent button Click update
      onEvent button Wheel setFlag

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)   = " "
cellToButtonStr (C _          Marked) = "M"
cellToButtonStr (C Mine       _)      = "¤"
cellToButtonStr (C (Nearby n) _)      = show n
