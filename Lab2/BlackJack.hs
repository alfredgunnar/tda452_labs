{-
  3.1:
  size hand2
  = size (Add (Card (Numeric 2) Hearts)(Add (Card Jack Spades) Empty))
  = 1 + size (Add (Card Jack Spades) Empty)
  = 1 + 1 + size Empty
  = 1 + 1 + 0
  = 2 + 0
  = 2
-}

module BlackJack where
import Cards
import RunGame


-- | A function that returns the empty Hand.
empty :: Hand
empty = Empty


-- | A function that calculates the value of a given Hand.
-- | Takes into account that Aces can be 1 or 11.
value :: Hand -> Integer
value Empty     = 0
value (Add c h) | valueCard c + value h <= 21 = valueCard c + value h
                | otherwise = valueCard c + value h
                  - (numberOfAces (Add c h) * 10)


-- | A function that calculates the value of a Rank.
valueRank :: Rank -> Integer
valueRank (Numeric r) = r
valueRank Ace         = 11
valueRank _           = 10
            

-- | A function that calculates the value of a Card.
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)


-- | A function that calculates the number of Aces in a given Hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | rank card /= Ace = numberOfAces hand


-- | A function that returns True if value of the Hand > 21. Otherwise False.
gameOver :: Hand -> Bool
gameOver h = value h > 21


-- | Given one Hand for the Guest and one for the Bank (in that order),
-- | this function returns the winner.
winner :: Hand -> Hand -> Player
winner hG hB | gameOver hG && not (gameOver hB) || value hB >= value hG
                         = Bank
             | otherwise = Guest
