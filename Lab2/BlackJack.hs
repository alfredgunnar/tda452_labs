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
import Test.QuickCheck hiding (shuffle)
import System.Random


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


-- | Given two hands, <+ puts the first one on top of the second one
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2 = h2
(<+) (Add c Empty) h2 = (Add c h2)
(<+) (Add c h1) h2 = (Add c (h1 <+ h2))


-- | <+ should be associative
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
   p1<+(p2<+p3) == (p1<+p2)<+p3


-- | The size of a combined hand should be the same as the sum of two
-- | individual hands.
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size h1 + size h2 == size (h1 <+ h2)


-- | Returns all 52 cards unshuffled
fullDeck :: Hand
fullDeck = h1 <+ h2 <+ h3 <+ h4
    where h1 = allCardsInRank Hearts
          h2 = allCardsInRank Spades
          h3 = allCardsInRank Diamonds
          h4 = allCardsInRank Clubs


-- | Returns all possible cards in a given suit
allCardsInRank :: Suit -> Hand
allCardsInRank s = allCardsInRank' s 2  -- 2 is start number


-- | Returns all cards from given integer representing the rank of a card
allCardsInRank' :: Suit -> Integer -> Hand
allCardsInRank' s num | num < 2   = allCardsInRank' s 2
                      | num <= 10 = (Add (Card (Numeric num) s)
                                      (allCardsInRank' s (num + 1)))
                      | num == 11 = (Add (Card Jack s)
                                      (allCardsInRank' s (num + 1)))
                      | num == 12 = (Add (Card Queen s)
                                      (allCardsInRank' s (num + 1)))
                      | num == 13 = (Add (Card King s)
                                      (allCardsInRank' s (num + 1)))
                      | num >= 14 = (Add (Card Ace s) Empty)


{-

Alternatively, use Rank instead of Integer:

allCardsInRank' :: Suit -> Rank -> Hand
allCardsInRank' s (Numeric num) | num <= 10 =
                                  (Add (Card (Numeric num) s)
                                    (allCardsInRank' s (Numeric (num + 1))))
                                | num == 11 =
                                  (Add (Card Jack s)
                                    (allCardsInRank' s Queen))
allCardsInRank' s r             | r == Queen =
                                  (Add (Card Queen s)
                                    (allCardsInRank' s King))
                                | r == King =
                                  (Add (Card King s)
                                    (allCardsInRank' s Ace))
                                | r == Ace =
                                  (Add (Card Ace s) Empty)
-}


-- | Given a deck and a hand, draw one card from the deck and put on the hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _        = error "draw: The deck is empty."
draw (Add c h) hand = (h, (Add c hand))


-- | Given a deck, the bank plays until a score of 16 or higher is achieved
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty


-- | Helper function for playBank that given a deck and Hand plays until
-- | the score is 16 or higher.
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand' < 16 = playBank' deck' bankHand'
                        | otherwise           = bankHand'
    where (deck',bankHand') = draw deck bankHand


-- | Shuffles a deck/hand
shuffle :: StdGen -> Hand -> Hand
shuffle g1 Empty = Empty
shuffle g1 h = (Add pc (shuffle g2 remh))
    where
        (randn, g2) = randomR (1, size h) g1
        (pc, remh)  = pickCard h randn


-- | Picks the n:th card in a deck and returns the card as well
-- | as the remaining deck
pickCard :: Hand -> Integer -> (Card, Hand)
pickCard h n | n > 0 && n <= (size h) = pickCard' h n Empty
             | otherwise              = error "pickCard: n is out of bounds."


-- | Takes a remaining hand and an integer as well as an already checked hand
pickCard' :: Hand -> Integer -> Hand -> (Card, Hand)
pickCard' (Add c h) 1 ch = (c, h <+ ch)
pickCard' (Add c h) n ch = pickCard' h (n-1) (Add c ch)


-- | Function that return True iff the card is in the hand.
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h


-- | If a card is in the unshuffled deck, it shuld also be in the
-- | shuffled deck
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h


-- | Checks that the size of the shuffled deck is the same as the unshuffled
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle g h)


implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation
