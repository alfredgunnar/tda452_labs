module Minesweeper.Test where

import Minesweeper
import Test.QuickCheck

instance Arbitrary Cell where
  arbitrary = do n <- elements [n | n <- [0..100]]
                 cell n

cell :: Int -> Gen Cell
cell n = frequency [ (n, return (C Mine Idle))
                  , (100-n, return (C (Nearby 0) Idle))]


instance Arbitrary Board where
  arbitrary = do row <- elements [n | n <- [1..30]]
                 col <- elements [n | n <- [1..30]]
                 prob <- elements [n | n <- [0..100]]
                 board row col prob

board :: Int -> Int -> Int -> Gen Board
board rows cols prob = do rows <- vectorOf rows (vectorOf cols (cell prob))
                          let b = setNearbyMarkers (Board rows)
                          return b

prop_increment_cell :: Cell -> Bool
prop_increment_cell (C Mine b) = incrementCell (C Mine b) == (C Mine b)
prop_increment_cell (C (Nearby n) b) =
  incrementCell (C (Nearby n) b) == (C (Nearby (n+1)) b)

{-prop_explode_board_at :: Int -> Int -> Board
prop_explode_board_at row col b = -}

{-prop_set_nearby_markers :: Board -> Bool
prop_set_nearby_markers b -}



-- | Compares two boards to see if they are equal but does not look at if the
-- | cells are clicked or not
(=?=) :: Board -> Board -> Bool
(=?=) b1 b2 = b1' == b2'
  where
    b1' = (map click (concat (rows b1)))
    b2' = (map click (concat (rows b2)))

prop_beq :: Board -> Board -> Bool
prop_beq b1 b2 = (b1 =?= b1) && (b2 =?= b2)
