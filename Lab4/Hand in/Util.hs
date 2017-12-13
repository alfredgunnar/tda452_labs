module Util where

-- | Given a list, and a tuple containing an index in the list and a new
-- | value, update the given list with the new value at the given index.
-- | This function is taken from lab 3.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _                               = []
(!!=) list (n, _)     | n > length list = error "Too big index"
(!!=) (x:xs) (n, val) | n == 0           = val:xs
                      | n > 0            = x:(!!=) xs (n-1, val)
                      | otherwise = error "Too small index"

-- | Extension to the map function where the parameter function takes an int
-- | representing the current list index
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f l = mapi' 0 f l
  where
    mapi' ::  Int -> (Int -> a -> b) -> [a] -> [b]
    mapi' i f []     = []
    mapi' i f (x:xs) = ((f i x):(mapi' (i+1) f xs))

test_mapi :: Int -> a -> (a,Int)
test_mapi n x = (x,n)
