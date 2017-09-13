-- aabdul5_330_lab3.hs
module Lab3 where

-- A function to convert lowercase letters to uppercase
toUpperOnlyLower :: Char -> Char
toUpperOnlyLower ch = if fromEnum ch < 123 && fromEnum ch > 96 then toUpper ch else ch

toUpper :: Char -> Char
toUpper ch = toEnum (fromEnum ch + offset)
offset = fromEnum 'A' - fromEnum 'a'


-- Tests for the previous function
test_toUpperOnlyLower_with_lowercase, test_toUpperOnlyLower_with_uppercase, test_toUpperOnlyLower_with_digit  :: Bool
test_toUpperOnlyLower_with_lowercase = fromEnum (toUpperOnlyLower 'a') == fromEnum 'A'
test_toUpperOnlyLower_with_uppercase = fromEnum (toUpperOnlyLower 'B') == fromEnum 'B'
test_toUpperOnlyLower_with_digit = fromEnum (toUpperOnlyLower '4') == fromEnum '4'

-- A function to convert a digit like ’9’ into its value, 9.
charToNum :: Char -> Int
charToNum ch = if fromEnum ch < 58 && fromEnum ch > 47 then fromEnum ch - 48 else fromEnum ch

-- Tests for the previous function
test_charToNum_with_digit, test_charToNum_with_letter :: Bool
test_charToNum_with_digit = fromEnum (charToNum '3') == 3
test_charToNum_with_letter = fromEnum (charToNum 'A') == fromEnum 'A'

-- A function that takes three strings and returns a single string.
onThreeLines :: String -> String -> String -> String
onThreeLines string1 string2 string3 = string1 ++ "\n" ++ string2 ++ "\n" ++ string3
