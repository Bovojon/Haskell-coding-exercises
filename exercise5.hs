-- Function that, given a triple, (a,b,c), gives the next triple as (b,c,a+b+c).
tribStep :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
tribStep (a, b, c) = (b, c, a+b+c)

-- Function that gives the Tribonacci triple.
tribTriple :: Integer -> (Integer, Integer, Integer)
tribTriple n
 | n == 0 = (0, 0, 1)
 | otherwise = tribStep (tribTriple (n-1))

-- Function that gives the first element of a triple of integers.
fstInTriple :: (Integer, Integer, Integer) -> Integer
fstInTriple (x,y,z) = x

-- Function composition that gives the n-th tribonacci number.
fastTrib :: Integer -> Integer
fastTrib = fstInTriple . tribTriple

-- New Enumarated type: Ranks.
data Ranks = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq, Show, Ord)

-- New Enumarated type: Suits.
data Suits = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Show, Ord)

-- Rank is a synonym for Ranks.
type Rank = Ranks

-- Suit is a synonym for Suits.
type Suit = Suits

-- PlayingCard is a product type.
-- There are two ways of building an element of PlayingCard.
-- One way is to give a regular Card with Rank and Suit; the other way is to give a Joker.
data PlayingCard = Card Rank Suit | Joker
                   deriving (Eq, Show, Ord)

-- Function that gives a list of cards that make four of a kind of an input rank.
fourOfAKind :: Rank -> [PlayingCard]
fourOfAKind x = [(Card r s) | r <- [x], s <- [Clubs, Diamonds, Hearts, Spades]]

-- Function that shows the higher card between two cards in PlayingCard.
-- Rank is checked before Suit.
-- For example, Card Jack Clubs > Card Two Hearts, but Card Two Clubs < Card Two Hearts.
-- If two cards are the same, that will be printed out too.
-- Done with Guards.
higherCard :: PlayingCard -> PlayingCard -> String
higherCard (Card a x) (Card b y)
 | a > b = "The higher card is " ++ show a ++ " of " ++ show x
 | a < b = "The higher card is " ++ show b ++ " of " ++ show y
 | a == b && x > y = "The higher card is " ++ show a ++ " of " ++ show x
 | a == b && x < y = "The higher card is " ++ show b ++ " of " ++ show y
 | a == b && x == y = "Two cards are the same."


-- Function that doubles all the elements of a list of integers and returns the result.
doubleAll :: [Integer] -> [Integer]
doubleAll ls = [x*2 | x <- ls]

-- Function that returns a list of all of the integer divisors of a positive integer passed to it.
divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..x], mod x y == 0]
