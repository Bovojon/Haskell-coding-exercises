import Control.Monad
import Data.Char

greaterThan7 :: [Int] -> [Bool]
greaterThan7 x = map more x where
  more x = x > 7

greaterThan7' :: [Int] -> [Bool]
greaterThan7' y = map (>7) y

greaterThan7p' :: [Int] -> [Bool]
greaterThan7p' s = map (\x -> x > 7) s

takefirst :: Int -> ([a] -> [a])
takefirst n = (\x -> take n x)

giveDouble :: IO()
giveDouble = do
  putStrLn("Enter your number: ")
  number <- getLine
  if number /= "" then
    do
      putStrLn("your doubled number is " ++ show((read number::Int) * 2))
      giveDouble
  else
    putStrLn("thanks")

highestDigit :: String -> Int
highestDigit x
  | [m | m <- x, isDigit(m) == True] == [] = -1
  | otherwise = read ([maximum [m | m <- x, isDigit(m) == True]])::Int

highestDigit1 :: String -> Maybe Int
highestDigit1 x
  | [m | m <- x, isDigit(m) == True] == [] = Nothing
  | otherwise = Just (read ([maximum [m | m <- x, isDigit(m) == True]])::Int)

myzip :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myzip func [] [] [] = []
myzip func (x:xs) (y:ys) (p:ps) = [func x y p] ++ myzip func xs ys ps
