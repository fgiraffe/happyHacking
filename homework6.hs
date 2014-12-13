#!/usr/bin/env runhaskell

-- Q0

(^^^) :: Int -> Int -> Int

m ^^^ 1 = m
m ^^^ n = m * m ^^^ (n - 1)



-- Q 4

-- my_and
-- are all values in list True?

my_and :: [Bool] -> Bool

my_and [] = True
my_and (False : _) = False
my_and (True : xs) = my_and xs


-- good
qand1 :: [Bool] -> Bool
qand1 [] = True
qand1 (b : bs) = b && qand1 bs

-- good
qand2 :: [Bool] -> Bool
qand2 [] = True
qand2 (b : bs)
    | b = and bs
    | otherwise = False

-- no good
qand3 :: [Bool] -> Bool
qand3 [] = False
qand3 (b : bs) = b && qand3 bs

-- no good
qand4 :: [Bool] -> Bool
qand4 [] = False
qand4 (b : bs) = b || qand4 bs

-- good
qand5 :: [Bool] -> Bool
qand5 [] = True
qand5 (b : bs)
    | b == False = False
    | otherwise = qand5 bs

-- No good
qand6 :: [Bool] -> Bool
qand6 [] = True
qand6 (b : bs) = b || qand bs

-- good
qand7 :: [Bool] -> Bool
qand7 [] = True
qand7 (b : bs) = qand7 bs && b

-- no good
qand :: [Bool] -> Bool
qand [] = True
qand (b : bs)
    | b = b
    | otherwise = qand bs





main :: IO ()
main = do

    let tl1 = []
    let tl2 = [True]
    let tl3 = [False]
    let tl4 = [True, True]
    let tl5 = [True, False]
    let tl6 = [False, False]


    putStr ( "should be True :" )
    putStrLn ( show (qand tl1) )

    putStr ( "should be True :" )
    putStrLn ( show (qand tl2) )

    putStr ( "should be False :" )
    putStrLn ( show (qand tl3) )

    putStr ( "should be True :" )
    putStrLn ( show (qand tl4) )

    putStr ( "should be False :" )
    putStrLn ( show (qand tl5) )

    putStr ( "should be False :" )
    putStrLn ( show (qand tl6) )

