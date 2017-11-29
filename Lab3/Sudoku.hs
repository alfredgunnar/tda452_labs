import Test.QuickCheck
import Data.Char
import Data.List

-------------------------------------------------------------------------

-- | Representation of sudoku puzzlese (allows some junk)
data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [[Nothing | _ <- [1..9]] | _ <-[1..9]]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = listIsOfLength9 (rows s)
               && all listIsOfLength9 (rows s)
               && all isAllowedSudokuVal (concat (rows s))
    where
        listIsOfLength9 :: [a] -> Bool
        listIsOfLength9 l = length l == 9

        isAllowedSudokuVal :: Maybe Int -> Bool
        isAllowedSudokuVal Nothing = True
        isAllowedSudokuVal (Just n)  = n >= 1 && n <= 9

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = isSudoku s && all isNumber (concat (rows s))
    where
        isNumber :: Maybe Int -> Bool
        isNumber Nothing = False
        isNumber _       = True


-------------------------------------------------------------------------

-- * B1

-- |b printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn (concatMap sudokuLineToString (rows s))

sudokuLineToString :: [Maybe Int] -> String
sudokuLineToString = foldr ((:) . maybeIntToChar) "\n"

maybeIntToChar :: Maybe Int -> Char
maybeIntToChar Nothing  = '.'
maybeIntToChar (Just n) = intToDigit n

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do text <- readFile file
                     return (Sudoku (map stringToListOfMaybeInts
                                          (lines text)))
   where
     stringToListOfMaybeInts :: String -> [Maybe Int]
     stringToListOfMaybeInts = map charToMaybeInt

     charToMaybeInt :: Char -> Maybe Int
     charToMaybeInt '.' = Nothing
     charToMaybeInt n | n >= '1' && n <= '9' = Just (digitToInt n)
                      | otherwise            =
                          error ("readSudoku: unable to parse file. " ++
                            "Cannot read: " ++ [n])

-------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell =  frequency [(9, elements [Nothing]), (1, rJustInt)]


rJustInt :: Gen (Maybe Int)
rJustInt = elements [Just n | n <- [1..9]]


sudoku :: Gen Sudoku
sudoku = do rows <- vectorOf 9 (vectorOf 9 cell)
            return (Sudoku rows)

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- vectorOf 9 (vectorOf 9 cell)
       return (Sudoku rows)


-- * C3

-- | check that each generated Sudoku is a Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku


-------------------------------------------------------------------------

-- | a list that should contain 9 cells
type Block = [Maybe Int]

-- * D1

-- | given a Block, check if that block does not contain the same digit twice
isOkayBlock :: Block -> Bool
isOkayBlock b = length (nubBy equalSpecial b) == 9
    where
        equalSpecial Nothing Nothing = False
        equalSpecial a b = a == b


-- * D2

-- | given a Sudoku, create a list of all blocks of that Sudoku
blocks :: Sudoku -> [Block]
blocks s = rows s ++ transpose (rows s) ++ extractBlocks (rows s)
  where
    extractBlocks :: [[Maybe Int]] -> [Block]
    extractBlocks [] = []
    extractBlocks remainingRows =
      extractBlocksFromRow (take 3 remainingRows)
        ++ extractBlocks (drop 3 remainingRows)

    extractBlocksFromRow :: [[Maybe Int]] -> [Block]
    extractBlocksFromRow [[],[],[]]   = []
    extractBlocksFromRow remain =
            concatMap (take 3) remain
              : extractBlocksFromRow (map (drop 3) remain)



prop_block_size :: Sudoku -> Bool
prop_block_size s = length (blocks s) ==
                      27 && all (\b->length b == 9) (blocks s)


-- * D3

-- | given a Soduko, checks that all rows, colums
-- | and 3x3 blocks do not contain the same digit twice
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)


-----------------------------------------------------------------------------


type Pos = (Int,Int)

-- * E1

-- | Given a sudoku, return the positions that are blank
blanks :: Sudoku -> [Pos]
blanks s = blanks' (rows s) 0

blanks' :: [[Maybe Int]] -> Int -> [Pos]
blanks' [] _ = []
blanks' (r:rs) n = (findBlanksOnRow r n) ++ (blanks' rs (n+1))

-- | Takes a list of sudoku elements and a row number to retrieve
-- | the blank positions
findBlanksOnRow :: [Maybe Int] -> Int -> [Pos]
findBlanksOnRow [] _ = []
findBlanksOnRow (x:xs) n | x == Nothing =
                            [(n, 8 - length xs)] ++ findBlanksOnRow xs n
                         | otherwise = findBlanksOnRow xs n

-- * E2

-- | given a list, and a tuple containing an index in the list and a new
-- | value, update the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _                               = []
(!!=) list (n, _)     | n > length list = error "Too big index"
(!!=) (x:xs) (n, val) | n == 0           = val:xs
                      | n > 0            = x:(!!=) xs ((n-1), val)
                      | otherwise = error "Too small index"


prop_replace_element :: Eq a => [a] -> (Int, a) -> Bool
prop_replace_element [] _ = True
prop_replace_element list (n, val) | n > length list =
    prop_replace_element list (n `mod` (length list), val)
                                   | n < 0           =
    prop_replace_element list ((-n), val)
                                   | otherwise       =
    length (list !!= (n, val)) == length list
                       && validateParts (splitAt (n-1) list)
                             (splitAt (n-1) (list !!= (n, val)))
                             val

validateParts :: Eq a => ([a], [a]) -> ([a], [a]) -> a -> Bool
validateParts (l1, (y:ys1)) (l2, (y2:ys2)) val = (l1 == l2)
                                                   && (val == y2)
                                                   && (ys1 == ys2)
