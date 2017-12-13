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
  , iClickAll  :: Board -> Board
  }

-- Convert a cell to string representation
cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)          = " "
cellToButtonStr (C _          Marked)        = "⚑"
cellToButtonStr (C Mine       _)             = "¤"
cellToButtonStr (C (Nearby n) _) | n == 0    = " "
                                 | otherwise = show n


-- This function takes a function that creates cell elements (newCellElem)
-- and also a board
getCellElems f b = do parent <- newElem "div"
                      children <- sequence (getCellElems' 0 f (rows b))
                      return children

-- This is a helper function for getCellElems that is used to call the cell
-- creator function for each row
getCellElems' n f []     = []
getCellElems' n f (r:rs) = children ++ (getCellElems' (n+1) f rs)
  where
    children = mapi (f n) r

-- Update all cells visually (sets class to "clicked" if applicable)
updateProperty [] [] = do return ()
updateProperty (btn:btns) ((C ct cs): bcs) =
   do setProp btn "value" (cellToButtonStr (C ct cs))
      if cs == Clicked
        then setClass btn "clicked" True
        else return ()
      updateProperty btns bcs

-- Interface between view and model
implementation = Interface
 { iBoard = rndBoard,
   iOpen = open,
   iHasWon = hasWon,
   iMarkAt = markAt,
   iWidth = width,
   iClickAll = clickAll
 }

main = runGame implementation

runGame i =
   do hello <- newTextElem "Minesweeper Deluxe Edition"
      header <- newElem "h1"
      appendChild header hello

      -- Generate initial board. Numbers are: rows, columns, and
      -- probability for a tile to have a mine [0-100]
      b <- (iBoard i 15 20 15)
      globalBoard <- newIORef b

      -- Set up gameOver variable
      globalGameOver <- newIORef False

      -- Calculate GUI width and set to containers
      let gameDivWidth = show (2 * (iWidth i b)) ++ "em"
      gameDiv <- newElem "div"
                  `with` [style "width" =: gameDivWidth]
      setClass gameDiv "gameDiv" True
      gameContainer <- newElem "div"
                     `with` [style "width" =: gameDivWidth]
      setClass gameContainer "gameContainer" True

      -- Radio buttons for open or put out a flag
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

      -- Keep track of radio buttons clicked with a class
      -- Were no hasAttribute class so we used hasClass instead
      let markClicked _ = do setClass radioOpen "clicked" True
                             setClass radioFlag "clicked" False

      let flagClicked _ = do setClass radioFlag "clicked" True
                             setClass radioOpen "clicked" False

      onEvent radioOpen Click markClicked
      onEvent radioFlag Click flagClicked

      -- This function sets the gameOver variable to true
      let announceWinner = do e <- newTextElem "WINNER!"
                              w <- newElem "p"
                              appendChild w e
                              setClass w "winner" True
                              appendChild gameContainer w
                              writeIORef globalGameOver True

      -- This functions also sets the gameOver variable to true
      let announceLoser = do e <- newTextElem "LOSER!"
                             l <- newElem "p"
                             appendChild l e
                             setClass l "loser" True
                             appendChild gameContainer l
                             writeIORef globalGameOver True

      -- Used when updating viewed board according to model
      let updateBoard board = do gameBtns <- (getChildren gameDiv)
                                 let boardCells = concat (rows board)
                                 updateProperty gameBtns boardCells
                                 writeIORef globalBoard board

      -- Called when radio button is open and the board is clicked
      let clickOpen row col board = do let b' = iOpen i row col board
                                       if not (isNothing b')
                                         then do let b'' = (fromJust b')
                                                 updateBoard b''

                                                 if (iHasWon i (fromJust b'))
                                                   then announceWinner
                                                   else return ()
                                         else do announceLoser
                                                 let b' = iClickAll i board
                                                 updateBoard b'

      -- Called when radio button is flag and the board is clicked
      let clickSetFlag row col board = do let b' = iMarkAt i row col board
                                          updateBoard b'

      -- Listens for click events on the cells
      let clickDetect row col _ = do gameOver <- readIORef globalGameOver
                                     board <- readIORef globalBoard
                                     if not gameOver
                                       then do ret <-
                                                hasClass radioOpen "clicked"
                                               if ret
                                                 then clickOpen row col board
                                                 else clickSetFlag row col board
                                       else do return ()

      -- Creates a cell and registers click listener
      let newCellElem row col c = do e <- newElem "input"
                                       `with` [attr "type" =: "button",
                                          attr "value" =: cellToButtonStr c]
                                     onEvent e Click (clickDetect row col)
                                     return e


      -- From this point, the rest is just layout handling
      gameBoard <- getCellElems newCellElem b

      container <- newElem "div"
      setClass container "container" True



      gameDivTop <- newElem "div"
                     `with` [style "width" =: gameDivWidth]

      setClass gameDivTop "gameDivTop" True

      smileyBtn <- newElem "button"
        `with` [attr "onClick" =: "location.reload()"]
      setClass smileyBtn "smileyBtn" True

      smiley <- newElem "img"
        `with` [attr "src" =: "smiley.png",
                attr "alt" =: ":)"]
      setClass smiley "smiley" True

      -- Add css stylesheet
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
