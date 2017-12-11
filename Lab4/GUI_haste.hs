import Haste.DOM
import Haste.Events
import Minesweeper
import Data.Maybe (fromJust, isNothing)

data Interface = Interface
  { iBoard    :: Int -> Int -> Int -> IO Board
  , iOpen     :: Int -> Int -> Board -> Maybe Board
  }


newBoardElem b = do parent <- newElem "div"
                    children <- sequence (newBoardElem' (rows b))
                    setChildren parent children --(newBoardElem' (rows b))
                    return parent


newBoardElem' []     = []
newBoardElem' (r:rs) = children ++ [newElem "br"] ++ (newBoardElem' rs)
  where
    children = map newCellElem r


newCellElem c = newElem "input"
                      `with` [attr "type" =: "button",
                              attr "value" =: cellToButtonStr c]

b1 = Board { rows = [[C Mine True,C (Nearby 1) True,C (Nearby 1) True],
                     [C (Nearby 0) True,C Mine False,C Mine False]]}

implementation = Interface
 { iBoard = rndBoard,
   iOpen = open
 }

main = runGame implementation

runGame i =
   do hello <- newTextElem "Hello"
      header <- newElem "h1"
      appendChild header hello
      appendChild documentBody header

      b <- (iBoard i 10 10 1)

      gameDiv <- newElem "div"
      appendChild documentBody gameDiv

      gameBoard <- newBoardElem b
      appendChild gameDiv gameBoard


      rowinput <- newElem "input"
        `with` [attr "id" =: "row"]
      rowlabeltxt <- newTextElem "Row"
      rowlabel <- newElem "label"
        `with` [attr "for" =: "row"]
      appendChild rowlabel rowlabeltxt

      colinput <- newElem "input"
        `with` [attr "id" =: "col"]
      collabeltxt <- newTextElem "Col"
      collabel <- newElem "label"
        `with` [attr "for" =: "col"]
      appendChild collabel collabeltxt

      output <- newElem "input"


      let reloadBoard r c b = do clearChildren gameDiv
                                 let b' = iOpen i r c b
                                 if not (isNothing b')
                                   then do gameBoard <- newBoardElem (fromJust b')
                                           appendChild gameDiv gameBoard
                                   else do e <- newTextElem "LOSER"
                                           appendChild documentBody e


      let update _ = do row <- getProp rowinput "value"
                        col <- getProp colinput "value"
                        let r = read row :: Int -- can fail!
                        let c = read col :: Int -- can fail!

                        reloadBoard r c b

                        setProp output "value" ("(" ++ show r ++ "," ++ show c ++ ")")

      button <- newElem "input"
                  `with` [attr "type" =: "button",
                          attr "value" =: "Update"]

      appendChild documentBody rowlabel
      appendChild documentBody rowinput
      appendChild documentBody collabel
      appendChild documentBody colinput
      appendChild documentBody button
      appendChild documentBody output

      onEvent button Click update

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          False) = " "
cellToButtonStr (C Mine       _)     = "¤"
cellToButtonStr (C (Nearby n) _)     = show n
