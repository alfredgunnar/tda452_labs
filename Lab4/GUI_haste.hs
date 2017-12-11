import Haste.DOM
import Haste.Events
import Test.QuickCheck
import Minesweeper

data Interface = Interface
  { iBoard    :: Int -> Int -> Int -> Gen Board
  , iOpen     :: Int -> Int -> Board -> Maybe Board
  }


newBoardElem b = do parent <- newElem "div"
                    children <- sequence (newBoardElem' (rows b))
                    setChildren parent children --(newBoardElem' (rows b))
                    return parent


newBoardElem' []     = []
newBoardElem' (r:rs) = children ++ [newElem "br"] ++ (newBoardElem' rs) --children : (newBoardElem' rs)
  where
    children = map newCellElem r


newCellElem c = newElem "input"
                      `with` [attr "type" =: "button",
                              attr "value" =: cellToButtonStr c]

b1 = Board { rows = [[C Mine True,C (Nearby 1) True,C (Nearby 1) True],
                     [C (Nearby 0) True,C Mine False,C Mine False]]}


main = do hello <- newTextElem "Hello"
          header <- newElem "h1"
          appendChild header hello
          appendChild documentBody header

          gameBoard <- newBoardElem b1
          appendChild documentBody gameBoard


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

          button <- newElem "input"
                    `with` [attr "type" =: "button",
                            attr "value" =: "Update"]

          appendChild documentBody rowlabel
          appendChild documentBody rowinput
          appendChild documentBody collabel
          appendChild documentBody colinput
          appendChild documentBody button
          appendChild documentBody output

          let update _ = do row <- getProp rowinput "value"
                            col <- getProp colinput "value"
                            let r = read row :: Int -- can fail!
                            let c = read col :: Int -- can fail!
                            setProp output "value" ("(" ++ show r ++ "," ++ show c ++ ")")

          onEvent button Click update



cellToButtonStr :: Cell -> String
cellToButtonStr (C _          False) = " "
cellToButtonStr (C Mine       _)     = "¤"
cellToButtonStr (C (Nearby n) _)     = show n
