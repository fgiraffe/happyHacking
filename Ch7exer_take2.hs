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


-- implement map and filter using foldr
-- my_map :: (a -> b) -> [a] -> [b]
-- my_map f = foldr f []

--
--
-- main :: IO ()
-- main = do
--     exer1
--     exer2
--     putStrLn "\nEnd of tests."