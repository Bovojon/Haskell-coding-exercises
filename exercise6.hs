---------------------------------------------------------------- 6.5

myfoldr1 f [a]    = a
myfoldr1 f (a:as) = f a (myfoldr1 f as)

---------------------------------------------------------------- 6.6

myfoldr f y [] = y
myfoldr f y (x:xs) = f x (myfoldr f y xs)

---------------------------------------------------------------- 6.7
myfilterA :: [a] -> (a->Bool) -> [a]
myfilterA ls f = [ x | x<-ls, f x == True ]

myfilterB :: [a] -> (a->Bool) -> [a]
myfilterB ls f = filter f ls

---------------------------------------------------------------- 6.8

titleMachine :: String -> (String->String)
titleMachine str = (\x -> str++x)

---------------------------------------------------------------- 6.9

binaryArgFlip :: (a->b->c) -> (b->a->c)
binaryArgFlip f a1 a2 = f a2 a1

---------------------------------------------------------------- 6.11

totalLA :: (Integer -> Integer) -> (Integer -> Integer)
totalLA f = (\x -> foldr1 (+) (map f [0..x]))

totalPA :: (Integer -> Integer) -> (Integer -> Integer)
totalPA f x = (f x) + (totalPA f (x-1))

double :: Integer -> Integer
double x = 2*x
---------------------------------------------------------------- 6.12

curry3 :: ((a,b,c) -> d) -> (a -> b -> c -> d)
curry3 g x y z = g (x,y,z)

uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (x,y,z) = f x y z

mul :: Int -> Int -> Int -> Int
mul x y z = x*y*z
