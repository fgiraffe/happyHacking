#!/usr/bin/env runhaskell

module Ch7exer_take2 where


-- ex 1; translate 
--     [f x | x <- xs, p x]
-- into same using map and filter

mf_higher :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mf_higher _ _ [] = [] 
mf_higher fm fp xs = map fm (filter fp xs)

    
-- Exercise 2: all, any, takeWhile, dropWhile

my_all p = and . map p
my_any p = or . map p

-- cribbed from lecture
-- note: cant just use filter because we return immedaitely if first fails
    
my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile fn [] = []
my_takeWhile fn (x:xs)   | fn x      = x : my_takeWhile fn xs
                        | otherwise = []


my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile fn [] = []
my_dropWhile fn (x:xs)   | fn x      = my_dropWhile fn xs
                        | otherwise = x : xs


-- had to look up answers here but I was getting stuck on syntax,
-- my idea of what to do was OK. :-/

-- implement map f and filter p using foldr
my_map :: (a -> b) -> [a] -> [b]
my_map f = foldr (\x xs -> f x : xs) []

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter p = foldr (\x xs -> if p x then x : xs else xs) []


-- Ex 4: dec2int using foldl
-- eg [2,3,4,5] -> 2345

dec2int :: [Int] -> Int
dec2int = foldl(\x y -> 10 * x + y) 0



-- ex 5: curry and uncurry

add_pairs :: (Int, Int) -> Int
add_pairs (x, y) = x + y

add_curried :: Int -> Int -> Int
add_curried x y = x + y

my_curry :: ((a, b) -> c) -> a -> b -> c
my_curry f = \x y -> f(x, y)

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry f = \(x, y) -> f x y

--
--
-- main :: IO ()
-- main = do
--
--    -- let fooUnCurryAns = (my_uncurry (my_curry (add_pairs 5 10)))
--
--
--     putStrLn (show pairsAns)
--     putStrLn (show curryAns)
--     putStrLn (show synthPairsAddAns)
--     putStrLn (show synthCurryAddAns)
--
--     putStrLn "\nEnd of tests."