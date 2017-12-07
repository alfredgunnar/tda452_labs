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
