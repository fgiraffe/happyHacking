#!/usr/bin/env runhaskell

module Lab3 where

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [ x | x <- xs, (mod x 2) == 0 ]

-- ===================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares :: ...
squares :: Integer -> [Integer]
squares 0 = []
squares n = [ x * x | x <- [0 .. n] ]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)


-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function
-- squares' :: ...
squares' :: Integer -> Integer -> [Integer]
squares' 0 _ = []
squares' count start = [ x * x | x <- [start + 1 .. start + count]]

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords xmax ymax = [ (x,y) | x <- [0 .. xmax], y <- [0 .. ymax]]

-- ###########################################
-- ###########################################

main :: IO ()
main = do

    let evTest1 = [0..10]
    let q0_src = [827305 .. 927104]

    let q0_ANS = sum . evens $ q0_src

--     putStrLn ("Lab Ex 0-2")
--     putStrLn ("Evens on: " ++ (show evTest1) ++ evTest1 )

    putStrLn ("Lab Ex 0-2")
    putStrLn ("Evens on: " ++ (show evTest1) )

    putStrLn ("Ans 1: " ++ (show q0_ANS) )


