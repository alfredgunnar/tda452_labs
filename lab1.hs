import Test.QuickCheck
-- Lab 1
-- Martin Arvedahl
-- Alfred Björk

-- Part 1

-- Answer: The number of steps is k+1 for all k.


-- Part 2

power1 n k = product (replicate(fromInteger(k)) (fromInteger(n)))


-- Part 3

power2 n k | k == 0 = 1
           | odd k  = n * (power2 n (k-1))
           | even k = power2 (n*n) (k `div` 2)


-- Part 4

-- A.
{-
   0^0 = 1      (testing k is 0)
   0^5 = 0      (testing k is odd)
   0^8 = 0      (testing k is even)

   2^0 = 1      (testing k is 0)
   2^5 = 32     (testing k is odd)
   2^8 = 256    (testing k is even)

   It is not necessary to test with a negative k due to the scope.
   Although, a negative n should work.

   -3^0 = 1     (testing k is 0)
   -3^5 = -243  (testing k is odd)
   -3^8 = -6561 (testing k is even)
-}


-- B.

prop_powers n k = prop_powers_tup (n,k)
prop_powers_tup (n,k) = (n^k == power1 n k) && (n^k == power2 n k)


-- C.

{-
   The following function test_powers covers all test
   cases for n ∈ [-100, 100] and k ∈ [0, 20], which
   includes the concrete examples stated in A.
-}

test_powers = and (map prop_powers_tup values)
    where values = [(n,k) | n<-[-100..100], k<-[0..20]]


-- D.





main = print $ test_powers
