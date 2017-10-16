firstHalf :: Char -> Bool
firstHalf ch
 | (fromEnum ch >= fromEnum 'A') && (fromEnum ch <= fromEnum 'M') = True
 | (fromEnum ch >= fromEnum 'a') && (fromEnum ch <= fromEnum 'm') = True
 | otherwise = False

countEarliesR :: String -> Int
countEarliesR st
 | length st == 0 = 0
 | firstHalf (head st) = 1 + countEarliesR (tail st)
 | otherwise = countEarliesR (tail st)


countEarliesC :: String -> Int
countEarliesC st = length ([ch | ch <- st, firstHalf ch])


atLeastOneTrue :: (Bool, Bool, Bool) -> Bool
atLeastOneTrue (True,_,_) = True
atLeastOneTrue (_,True,_) = True
atLeastOneTrue (_,_,True) = True
atLeastOneTrue (_,_,_) = False


atLeastOneTrueDiff :: (Bool, Bool, Bool) -> Bool
atLeastOneTrueDiff (False,False,False) = False
atLeastOneTrueDiff (_,_,_) = True



notDivBy3 = [x | x <- [1..], mod x 3 /= 0]

rect = [(a,b,c,d) | a <- [1..], b <- [1..a], c <- [1..a], d <- [1..a], (a*a + b*b == c*c + d*d) || (a*a + c*c == b*b + d*d) || (a*a + d*d == b*b + c*c)]
quad = [(a,b,c,d) | a <- [1..], b <- [1..a], c <- [1..a], d <- [1..a], (a*a) < 3 * ((b*b) + (c*c) + (d*d))]

firstNames = ["David","Wayne","Michael","Simon","Nick"]
lastNames = ["Beckham","Rooney","Carrick","Hunter","Williams","Giggs"]

humanNames  = [a ++ " " ++ b | a <- firstNames, b <-lastNames]

vowels = ['a','e','o','i','u','y']

cvc = [[a, b, c] | a <- ['a'..'z'], not (a `elem` vowels), c <- ['a'..'z'], not (c `elem` vowels), b <- vowels]

planetX = [[a,b,c,d] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z'], d <- ['a'..'z'], a /= b, a /= c, a /= d, b /= c, b /= d, c /= d]

planetY = [[a,b,c,d] | a <- ['a'..'z'], b <- ['a'..'z'], c <- ['a'..'z'], d <- ['a'..'z'], a /= b, a /= c, a == d, b /= c]



noVowels :: String -> String
noVowels st = [x | x <- st, not (x `elem` ['a','e','o','i','u','y','A','E','O','I','U','Y'])]

countVowels :: String -> Int
countVowels st = length([x | x <- st, x `elem` ['a','e','o','i','u','y','A','E','O','I','U','Y']])

interInt :: [Integer] -> [Integer] -> [Integer]
interInt ls1 ls2 = [x | x <- ls1, elem x ls2]

palindromes :: [String] -> [String]
palindromes ls = [x | x <- ls, x == reverse x]
