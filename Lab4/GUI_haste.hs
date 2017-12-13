import Haste.DOM
import Haste.Events
import Haste.LocalStorage
import Haste.Serialize
import Haste.JSON
import Data.IORef
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

--instance Serialize Cell where
--  toJSON :: a -> JSON
--  toJSON cell = show cell
--  fromJSON :: JSON -> Parser a
--  fromJSON (Num json) = show ""

--instance Serialize CellType where
----  toJSON :: a -> JSON
--  toJSON Mine = (Num (fromIntegral(-1)))
--  toJSON (Nearby n) = (Num (fromIntegral n))
----  parseJSON :: JSON -> Parser a
--  parseJSON (Num n) | n == -1 = Mine
--                   | n > -1  = Nearby n

implementation = Interface
 { iBoard = rndBoard,
   iOpen = open
 }

main = runGame implementation

a = rows b1

runGame i =
   do hello <- newTextElem "Hello"
      header <- newElem "h1"
      appendChild header hello
      appendChild documentBody header

      b <- (iBoard i 10 10 1)
      globalBoard <- newIORef b

      --setItem "minesweeper_board" (toJSON a)

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
                                           writeIORef globalBoard (fromJust b')
                                           appendChild gameDiv gameBoard
                                   else do e <- newTextElem "LOSER"
                                           appendChild documentBody e


      let update _ = do row <- getProp rowinput "value"
                        col <- getProp colinput "value"
                        let r = read row :: Int -- can fail!
                        let c = read col :: Int -- can fail!

                        board <- readIORef globalBoard
                        reloadBoard r c board

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
