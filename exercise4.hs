-- aabdul5_330_lab4.hs
module Lab34 where
  import Test.QuickCheck
  import Pictures

  -- Use this horse when testing fourPics
  horse1 = flipV horse

  -- A function to tell who is in the CS faculty
  csFacultyMember :: String -> Bool
  csFacultyMember "Charlie" = True
  csFacultyMember "Xunfei" = True
  csFacultyMember "Dave" = True
  csFacultyMember person = False

  -- A function to give the other side of a Heads or Tails of a coin
  otherSideOfCoin :: String -> String
  otherSideOfCoin "Heads" = "Tails"
  otherSideOfCoin "Tails" = "Heads"
  otherSideOfCoin other = "Please enter Heads or Tails"

  -- Different ways to complete the definition of fourPics
  fourPics1 :: Picture -> Picture
  fourPics1 pic =
      top `above` bottom
        where
          top  = pic `beside` invertColour (flipV pic)
          bottom = invertColour pic `beside` flipV pic

  fourPics2 :: Picture -> Picture
  fourPics2 pic =
      left `beside` right
        where
          left    = pic `above` inverted
          right   = invertedAndFlipped `above` flipped
          inverted = invertColour pic
          invertedAndFlipped = invertColour flipped
          flipped = flipV pic

  -- Enumerted types for travel modes
  data Travel = Car | Bike | Rollerblades
                deriving (Show, Eq)

  -- How fast is each travel mode
  fast :: Travel -> String
  fast Car = "Really Fast"
  fast Bike = "Fast"
  fast Rollerblades = "Not so fast"

  -- Which other modes does each mode beat
  beat :: Travel -> String
  beat Car = "Bike and Rollerblades"
  beat Bike = "Only Rollerblades"
  beat Rollerblades = "Slower Rollerblades"

  -- It should return the product of all of the values between m and n, inclusive. Your function should return 0 if m is smaller than n.
  rangeProduct :: Int -> Int -> Int
  rangeProduct m n
    | m < n = 0
    | m == n = n
    | m > n = m * (rangeProduct (m-1) n)

  -- A function to multiply using recursion and addition
  recurseMult :: Int -> Int -> Int
  recurseMult m n
    | n == 1 = m
    | n > 1 = m + (recurseMult m (n-1))
