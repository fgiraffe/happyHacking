#!/usr/bin/env runhaskell

-- examples and exercises from Chapter 5 of Programming in Haskell
-- all about list comprehensions

import Data.Char

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n  = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- creates adjacent pairs of list elements
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

-- build on pairs to see if a list is sorted

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]


-- find positions of an element of a list
-- lists do not have indicies, just head and tail

-- How positions works:
-- 1. creates a list of pairs by zipping 0..n with
--   each element in the src list.
-- then filters this list with the x == x'

positions :: Eq a => a -> [a] -> [Int]
positions x xs =
	[i | (x', i) <- zip xs [0..n], x == x']
	where n = length xs - 1

-- strings are just syntactic sugar for lists of char
-- so all list comp functions apply to strings

-- count the lower case letters in a string

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]



replicate :: Int -> a -> [a]
replicate n x  = [ x | y <- [1..n], x <- [x]]

-- return all triples that create a pythagorean set up to n

isPyth :: Int -> Int -> Int -> Bool
isPyth x y z = (z^2) == (x^2) + (y^2)

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], isPyth x y z]


-- return all perfect numbers up to n

isPerfect :: Int -> Bool
isPerfect x = (sum flist) == x
                where
                    facts = factors x
                    flist = take ((length facts) - 1) facts

perfects :: Int -> [Int]
perfects n = [ x | x <- [1..n], isPerfect x]

-- Q5
--
-- convert [(x, y) | x <- [1,2,3], y <- [4,5,6]] to form using
-- two comprehensions using a single generator
-- > [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

gen_example :: Num a => [a] -> [a] -> [(a,a)]
gen_example xs ys = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]


-- Q6

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions_w_find :: (Eq a) => a -> [a] -> [Int]
positions_w_find x xs = find x (zip xs [0 .. n])
    where n = length xs - 1

--  let test_src = [99, 1, 99, 3, 99, 2, 99, 99]
--  let poss = positions_w_find 99 test_src


-- Q7
scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]



main :: IO ()
main = do
    let foo = factors 40
    putStrLn (show foo)

    let primesTil60 = primes 60
    putStrLn (show primesTil60)

