import Test.QuickCheck
import Data.Char hiding (isNumber)
import Data.List
import Data.Set (Set)
import Data.Maybe (fromJust)
import qualified Data.Set as Set

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
       let s = Sudoku rows
       if isOkay s
         then return s
         else arbitrary



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

pos :: Gen Pos
pos = do row <- elements [0..8]
         col <- elements [0..8]
         return (row,col)
{-

   How to use this for types? We get error:

  * Illegal instance declaration for `Arbitrary Pos'
      (All instance types must be of the form (T t1 ... tn)
       where T is not a synonym.
       Use TypeSynonymInstances if you want to disable this.)
  * In the instance declaration for `Arbitrary Pos'


instance Arbitrary Pos where
 arbitrary = pos
-}

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
findBlanksOnRow (Nothing:xs) n = [(n, 8 - length xs)] ++ findBlanksOnRow xs n
findBlanksOnRow (x:xs) n       = findBlanksOnRow xs n

-- | Takes a list of sudoku elements to retrieve the numbers on that row
findNumsOnRow :: [Maybe Int] -> [Int]
findNumsOnRow [] = []
findNumsOnRow ((Just x):xs) = x:(findNumsOnRow xs)
findNumsOnRow (Nothing:xs) = findNumsOnRow xs

-- * E2

-- | Given a list, and a tuple containing an index in the list and a new
-- | value, update the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _                               = []
(!!=) list (n, _)     | n > length list = error "Too big index"
(!!=) (x:xs) (n, val) | n == 0           = val:xs
                      | n > 0            = x:(!!=) xs ((n-1), val)
                      | otherwise = error "Too small index"


-- | Checks that the length of the lists is the same before and after !!=.
-- | Also checks that the replaced element is the one we wanted and that the
-- | list is otherwise left untouched.
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

-- | Helper function that given two lists of the same length checks that
-- | the element has been replaced and that the rest of the list has remained
-- | the same.
validateParts :: Eq a => ([a], [a]) -> ([a], [a]) -> a -> Bool
validateParts (l1, (y:ys1)) (l2, (y2:ys2)) val = (l1 == l2)
                                                   && (val == y2)
                                                   && (ys1 == ys2)



-- * E3

-- | Get the value of a certain position in a given Sudoku
getVal :: Sudoku -> Pos -> Maybe Int
getVal s (row, col) = ((rows s) !! row) !! col

-- | Given a Sudoku, a position, and a new cell value, updates the given
-- | Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (row,column) cell =
  Sudoku (rows s !!= (row,((rows s) !! row) !!= (column,cell)))

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update sud (r,c) cell = newVal == cell
                                && newSudWOVal == origSudWOVal
  where row = abs r `mod` 9
        col = abs c `mod` 9
        newSud = update sud (row,col) cell
        newVal  = getVal newSud (row, col)
        newSudWOVal = (rows newSud) !!=
          (row, deleteAt col (rows newSud) !! row)
        origSudWOVal = rows sud !!=
          (row, deleteAt col (rows newSud) !! row)


-- | Helper function that removes elem at position n in a list
deleteAt :: Int -> [a] -> [a]
deleteAt n list = l ++ r
  where (l, _:r) = splitAt n list



-- * E4

-- | Given a Sudoku, and a blank position, determine which numbers could be
-- | legally written into that position. Returns empty list for non-blank
-- | position.
candidates :: Sudoku -> Pos -> [Int]
candidates s (row,col) | isNumber (getVal s (row,col))
                                   = []
                       | otherwise =
                         Set.toList (Set.fromList [1..9] `Set.difference` nonCandidates)
  where valuesFromRow    =
          Set.fromList (findNumsOnRow (rows s !! row))
        valuesFromColumn =
          Set.fromList (findNumsOnRow ((transpose (rows s)) !! col))
        valuesFromBlock  = Set.fromList
          (findNumsOnRow ((extractBlocks (rows s)) !! (
            (row `div` 3) * 3 + (col `div` 3))))
        nonCandidates    = ((valuesFromRow `Set.union` valuesFromColumn)
                            `Set.union` valuesFromBlock)

prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates s (r,c) = isSudoku s
                           && isOkay s
                           && and
                           (map (prop_update_valid_sudoku s (row,col)) cand)
  where cand = [Just n | n <- candidates s (row,col)]
        row = abs r `mod` 9
        col = abs c `mod` 9


prop_update_valid_sudoku :: Sudoku -> Pos -> Maybe Int -> Bool
prop_update_valid_sudoku s p v = isOkay newSud && isSudoku newSud
  where newSud = update s p v


-----------------------------------------------------------------------------

-- * F1

-- | Given a valid Sudoku that represents a 9x9 sudoku and has no blocks
-- | (rows, columns, 3x3 blocks) that contain the same digit twice, the
-- | function tries to solve the given Sudoku. Returns Nothing for an invalid
-- | argument.
solve :: Sudoku -> Maybe Sudoku
solve s | isSudoku s && isOkay s  = solve' (Just s) b (candidates s (head b))
        | otherwise = Nothing
  where
    b = blanks s

solve' :: Maybe Sudoku -> [Pos] -> [Int] -> Maybe Sudoku
solve' s [] (c:cs)      = s
solve' s (b:bs) []      = Nothing
solve' s (b:[]) (c:cs) | isOkay newSud = Just newSud
                       | otherwise = nextCand
 where
   newSud = update (fromJust s) b (Just c)
   nextCand = solve' s [b] cs

solve' s (b:bs) (c:cs) | nextPos == Nothing = nextCand
                       | otherwise = nextPos
  where
    newSud = update (fromJust s) b (Just c)
    nextPos = solve' (Just newSud) bs (candidates newSud (head bs))
    nextCand = solve' s (b:bs) cs


-- * F2


-- | Produces instructions for reading the Sudoku from the given file,
-- | solving it, and printing the answer.
readAndSolve :: FilePath -> IO ()
readAndSolve f = do s <- readSudoku f
                    let sol = solve s
                    if sol == Nothing
                      then putStrLn "(no solution)"
                      else printSudoku (fromJust sol)

-- * F3
-- | checks, given two Sudokus, whether the first one is a solution (i.e. all
-- | blocks are okay, there are no blanks), and also whether the first one
-- | is a solution of the second one (i.e. all digits in the second
-- | sudoku are maintained in the first one)
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solved notSolved | not (isOkay solved) && length (blanks solved) > 0
                                          = False
                              | otherwise = checkEqualityOfAddedCells
                                  (concat (rows solved))
                                  (concat (rows notSolved))

-- | The first list can have digits where the second list has "Nothing"
-- | However, where the second list has a digit, the first list got to have
-- | the same digit
checkEqualityOfAddedCells :: [Maybe Int] -> [Maybe Int] -> Bool
checkEqualityOfAddedCells [] [] = True
checkEqualityOfAddedCells (x:xs) (y:ys)
              | y == Nothing = checkEqualityOfAddedCells xs ys
              | otherwise    = x == y && checkEqualityOfAddedCells xs ys


-- * F4
-- | says that the function solve is sound. Soundness means that every
-- | supposed solution produced by solve actually is a valid solution
-- | of the original problem.
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s ==> isOkay (fromJust (solve s))
