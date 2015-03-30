#!/usr/bin/env runhaskell

-- sum [x ^ 2 | x <- [1 .. 5]]

-- replicate_fg n a = [a | _ <- [1 .. n]]
-- just testing 
-- let foo = replicate_fg 5 "yo"

-- Q4
--    let answ_ref = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

--    let poss_ans1 = [z | z <- [ [(x, y) | y <- [4,5,6] | x <- [1,2,3]] ]
-- no, braces do not balance

--    let poss_ans2 = concat [[[(x,y) | x <- [1,2,3]] | y <- [4,5,6]]
-- no, braces do not balance

--    let poss_ans3 = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]
-- winner!

-- Q5

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 .. n])
    where n = length xs - 1

--  let test_src = [99, 1, 99, 3, 99, 2, 99, 99]
--  let poss = positions 99 test_src


-- Q6
-- reference func

-- sum ( (xs !! i) * (ys !! i) ) for i = 0 to n-1

scalarproduct :: [ Int ] -> [ Int ] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]
-- winner!

-- scalarproduct xs ys = sum [x * y | x <- xs, y <- ys]
-- no, this sums the individual lists THEN multiples that sum


-- scalarproduct xs ys = product (zipWith (+) xs ys)
-- 315
-- not sure HOW this gets so big


-- Q12

-- takes two lists of the same length
-- and interleaves their elements in turn about order
-- riffle [1,2,3] [4,5,6] -> [1,4,2,5,3,6]

-- xs ys = [ x : [y] | x <- xs, y <- ys ]
-- nope makes [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [ [x,y] | (x, y) <- xs `zip` ys ]
-- winner


-- Q 13
-- divides 15 2 = False
-- divides 15 3 = True



divides :: Int -> Int -> Bool
divides dividend divisor =
    remain == 0 where remain = dividend `mod` divisor

divisors :: Int -> [Int]
divisors x = [d | d <- [1 .. x], x `divides` d]

main :: IO ()
main = do

    putStrLn (show (riffle [1,2,3] [4,5,6]))
    putStrLn ("Homework 5: Done.")


