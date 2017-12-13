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
        `with` [style "font-family" =: "Comic Sans MS",
                style "text-align" =: "center"]
      appendChild header hello

      --output <- newElem "input"

      b <- (iBoard i 10 10 8)
      globalBoard <- newIORef b

      let gameDivWidth = show (2 * (iWidth i b)) ++ "em"

      gameDiv <- newElem "div"
                  `with` [style "width" =: gameDivWidth,
                          style "border-top"    =: "2px solid #7B7B7B",
                          style "border-left"   =: "2px solid #7B7B7B",
                          style "border-right"  =: "2px solid #FFFFFF",
                          style "border-bottom" =: "2px solid #FFFFFF",
                          style "margin" =: "auto"]

      let onEventForElems event handler []       = do return ()
      let onEventForElems event handler (e:es) = (onEvent e event handler) : onEventForElems event handler es

      let setProp' propid val e = setProp e propid val

      let clickDetect row col _ = do board <- readIORef globalBoard
                                     let b' = iOpen i row col board



                                     if not (isNothing b')
                                       then do gameBtns <- (getChildren gameDiv)

                                               let b'' = (fromJust b')
                                               let boardCells = concat (rows b'')

                                               updateProperty gameBtns boardCells

                                               writeIORef globalBoard (fromJust b')

                                               if (iHasWon i (fromJust b'))
                                                 then do e <- newTextElem "WINNER!"
                                                         appendChild gameDiv e
                                                 else return ()

                                       else do e <- newTextElem "LOSER"
                                               appendChild gameDiv e

      let setFlag row col _ = do board <- readIORef globalBoard
                                 let b' = iMarkAt i row col board
                                 gameBtns <- (getChildren gameDiv)

                                 let boardCells = concat (rows b')

                                 updateProperty gameBtns boardCells
                                 --setProp output "value" "flag!"
                                 writeIORef globalBoard b'

      let newCellElem row col c = do e <- newElem "input"
                                       `with` [attr "type" =: "button",
                                             attr "value" =: cellToButtonStr c,
                                             attr "row" =: show row,
                                             attr "col" =: show col,
                                             style "width" =: "2em",
                                             style "height" =: "2em",
                                             style "border-top"    =: "2px solid #FFFFFF",
                                             style "border-left"   =: "2px solid #FFFFFF",
                                             style "border-right"  =: "2px solid #7B7B7B",
                                             style "border-bottom" =: "2px solid #7B7B7B",
                                             style "background-color" =: "#BDBDBD",
                                             style "font-size"  =: "inherit"]
                                     onEvent e Click (clickDetect row col)
                                     onEvent e Wheel (setFlag row col)
                                     return e


      gameBoard <- getCellElems newCellElem b

      container <- newElem "div"
        `with` [style "margin" =: "auto",
                style "width"  =: "80%",
                style "font-size"  =: "100%"
                ]

      gameContainer <- newElem "div"
        `with` [style "text-align" =: "center",
                style "margin" =: "auto",
                style "padding" =: "10px",
                style "background-color" =: "#BDBDBD",
                style "width"  =: gameDivWidth,
                style "height"  =: "100%",
                style "border"  =: "2px solid #7B7B7B",
                style "font-size"  =: "100%"

                ]

      gameDivTop <- newElem "div"
        `with` [style "text-align" =: "center",
                style "margin" =: "auto",
                style "width"  =: gameDivWidth,
                style "border-top"    =: "2px solid #7B7B7B",
                style "border-left"   =: "2px solid #7B7B7B",
                style "border-right"  =: "2px solid #FFFFFF",
                style "border-bottom" =: "2px solid #FFFFFF",
                style "height"  =: "4em",
                style "background-color"  =: "#C0C0C0",
                style "font-size"  =: "100%"
                ]

      smileyBtn <- newElem "button"
        `with` [style "margin-top" =: "1.2em",
                style "padding" =: "2px 0px 0px",
                style "width"  =: "26px",
                style "height"  =: "26px",
                style "font-size"  =: "100%",
                style "border-top"    =: "2px solid #FFFFFF",
                style "border-left"   =: "2px solid #FFFFFF",
                style "border-right"  =: "2px solid #7B7B7B",
                style "border-bottom" =: "2px solid #7B7B7B",
                style "background-color" =: "#BDBDBD",
                style "font-size"  =: "inherit"
                ]
      smiley <- newElem "img"
        `with` [attr "src" =: "smiley.png",
                attr "alt" =: ":)"]

      appendChild documentBody container
      appendChild container header
      appendChild container gameContainer
      appendChild gameContainer gameDivTop
      appendChild gameDivTop smileyBtn
      appendChild smileyBtn smiley
      appendChild gameContainer gameDiv
      setChildren gameDiv gameBoard
      --appendChild documentBody output

cellToButtonStr :: Cell -> String
cellToButtonStr (C _          Idle)   = " "
cellToButtonStr (C _          Marked) = "M"
cellToButtonStr (C Mine       _)      = "¤"
cellToButtonStr (C (Nearby n) _)      = show n
