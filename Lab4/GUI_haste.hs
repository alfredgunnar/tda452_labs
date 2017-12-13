import Haste.DOM
import Haste.Events
import Data.IORef
import Minesweeper
import Data.Maybe (fromJust, isNothing)
import Util

data Interface = Interface
  { iBoard     :: Int -> Int -> Int -> IO Board
  , iOpen      :: Int -> Int -> Board -> Maybe Board
  , iHasWon    :: Board -> Bool
  , iMarkAt    :: Int -> Int -> Board -> Board
  , iWidth     :: Board -> Int
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
updateProperty (btn:btns) ((C ct cs): bcs) = do setProp btn "value" (cellToButtonStr (C ct cs))
                                                if cs == Clicked
                                                  then setClass btn "clicked" True
                                                  else return ()
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
      appendChild header hello

      b <- (iBoard i 15 20 15)
      globalBoard <- newIORef b

      globalGameOver <- newIORef False

      let gameDivWidth = show (2 * (iWidth i b)) ++ "em"

      gameDiv <- newElem "div"
                  `with` [style "width" =: gameDivWidth]
      setClass gameDiv "gameDiv" True

      gameContainer <- newElem "div"
                     `with` [style "width" =: gameDivWidth]
      setClass gameContainer "gameContainer" True


      radioOpen <- newElem "input"
        `with` [attr "type" =: "radio",
                attr "name" =: "radio",
                attr "id" =: "radioOpen",
                attr "value" =: "open",
                attr "checked" =: ""]
      setClass radioOpen "clicked" True

      radioFlag <- newElem "input"
        `with` [attr "type" =: "radio",
                attr "name" =: "radio",
                attr "id" =: "radioFlag",
                attr "value" =: "flag"]

      let markClicked _ = do setClass radioOpen "clicked" True
                             setClass radioFlag "clicked" False

      let flagClicked _ = do setClass radioFlag "clicked" True
                             setClass radioOpen "clicked" False

      onEvent radioOpen Click markClicked
      onEvent radioFlag Click flagClicked




      let announceWinner = do e <- newTextElem "WINNER!"
                              w <- newElem "p"
                              appendChild w e
                              setClass w "winner" True
                              appendChild gameContainer w
                              writeIORef globalGameOver True

      let announceLoser = do e <- newTextElem "LOSER!"
                             l <- newElem "p"
                             appendChild l e
                             setClass l "loser" True
                             appendChild gameContainer l
                             writeIORef globalGameOver True

      let clickOpen row col board = do let b' = iOpen i row col board
                                       if not (isNothing b')
                                         then do gameBtns <- (getChildren gameDiv)
                                                 let b'' = (fromJust b')
                                                 let boardCells = concat (rows b'')
                                                 updateProperty gameBtns boardCells
                                                 writeIORef globalBoard (fromJust b')
                                                 if (iHasWon i (fromJust b'))
                                                   then announceWinner
                                                   else return ()
                                         else announceLoser

      let setFlag row col board = do let b' = iMarkAt i row col board
                                     gameBtns <- (getChildren gameDiv)
                                     let boardCells = concat (rows b')
                                     updateProperty gameBtns boardCells
                                     writeIORef globalBoard b'

      let clickDetect row col _ = do gameOver <- readIORef globalGameOver
                                     if not gameOver
                                       then do board <- readIORef globalBoard
                                               ret <- hasClass radioOpen "clicked"
                                               if ret
                                                 then clickOpen row col board
                                                 else setFlag row col board
                                       else return ()


      let newCellElem row col c = do e <- newElem "input"
                                       `with` [attr "type" =: "button",
                                             attr "value" =: cellToButtonStr c]
                                     onEvent e Click (clickDetect row col)
                                     return e


      gameBoard <- getCellElems newCellElem b

      container <- newElem "div"
      setClass container "container" True



      gameDivTop <- newElem "div"
                     `with` [style "width" =: gameDivWidth]

      setClass gameDivTop "gameDivTop" True

      smileyBtn <- newElem "button"
      setClass smileyBtn "smileyBtn" True

      smiley <- newElem "img"
        `with` [attr "src" =: "smiley.png",
                attr "alt" =: ":)"]
      setClass smiley "smiley" True

      c <- getChildren document
      let html = c !! 0
      h <- getChildren html
      let headerEl = h !! 0
      style <- newElem "link"
          `with` [attr "rel" =: "stylesheet",
                  attr "type" =: "text/css",
                  attr "href" =: "style.css"]

      radioContainer <- newElem "div"
      setClass radioContainer "radioContainer" True



      --setAttr documentBody "oncontextmenu" "return false;"

      appendChild headerEl  style

      appendChild documentBody container
      appendChild container header
      appendChild container gameDiv
      appendChild documentBody container
      appendChild container header
      appendChild container gameContainer
      appendChild gameContainer gameDivTop
      appendChild gameDivTop smileyBtn
      appendChild smileyBtn smiley
      appendChild gameContainer gameDiv
      setChildren gameDiv gameBoard

      appendChild container radioContainer

      openLabel <- newElem "label"
        `with` [attr "for" =: "radioOpen"]
      openLabelText <- newTextElem "Open"
      appendChild openLabel openLabelText


      flagLabel <- newElem "label"
        `with` [attr "for" =: "radioFlag"]
      flagLabelText <- newTextElem "Flag"
      appendChild flagLabel flagLabelText

      appendChild radioContainer radioOpen
      appendChild radioContainer openLabel

      appendChild radioContainer radioFlag
      appendChild radioContainer flagLabel

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)   = " "
cellToButtonStr (C _          Marked) = "M"
cellToButtonStr (C Mine       _)      = "¤"
cellToButtonStr (C (Nearby n) _)      | n == 0 = " "
                                      | otherwise = show n
