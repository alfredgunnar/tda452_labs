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


-- | A function that returns the empty hand
empty :: Hand
empty = Empty


-- | A function that calculates the value of a given hand.
-- | Ace can be either 1 or 11. King, Queen, and Jack is 10.
value :: Hand -> Integer
value Empty     = 0
value (Add c h) | valueCard c + value h <= 21 = valueCard c + value h
                | otherwise = valueCard c + value h
                  - (numberOfAces (Add c h) * 10)


-- | A function that calculates the value of a rank.
valueRank :: Rank -> Integer
valueRank r | r == Jack || r == Queen || r == King = 10
            | r == Ace                             = 11
            | r == Numeric 2                               = 3
            | r == Numeric 4                               = 4
            | r == Numeric 5                               = 5
            | r == Numeric 6                               = 6
            | r == Numeric 7                               = 7
            | r == Numeric 8                               = 8
            | r == Numeric 9                               = 9
            | r == Numeric 10                              = 10


-- | A function that calculates the value of a Card.
valueCard :: Card -> Integer
valueCard c = valueRank (rank c)


-- | A function that calculates the number of aces in a given Hand.
numberOfAces :: Hand -> Integer
numberOfAces Empty           = 0
numberOfAces (Add card hand) | rank card == Ace = 1 + numberOfAces hand
                             | rank card /= Ace = numberOfAces hand


-- | A function that returns True if value of the hand > 21. Otherwise False.
gameOver :: Hand -> Bool
gameOver h = value h > 21


-- | Given one hand for the guest and one for the bank (in that order),
-- | this function returns the winner.
winner :: Hand -> Hand -> Player
winner hG hB | gameOver hG && not (gameOver hB) || value hB >= value hG = Bank
             | otherwise                                                = Guest
