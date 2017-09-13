-- aabdul5_330_lab1.hs
module FirstScript where

-- A value set to the String exclamation mark
exclamation :: String
exclamation = "!"

-- The function to square an Integer
square :: Integer -> Integer
square n = n*n

-- The function to cube an integer
cube :: Integer -> Integer
cube n = n*n*n

-- A function composition to square an integer first and then cube the result.
composite :: Integer -> Integer
composite = square . cube

-- A function to concatenate a string with itself
string_to_itself :: String -> String
string_to_itself n = n ++ n

-- A function to concatenate a string with an exclamation mark
add_exclamation :: String -> String
add_exclamation n = n ++ exclamation
