#!/usr/bin/env runhaskell

--fibs2 :: [Integer]
--fibs2 = 0 : 1 : zipWith (*) fibs2 (tail fibs2)
--
fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

fib1 :: Int -> Integer
fib1 n = last (take n fibs)

fib2 :: Int -> Integer
fib2 n = head (drop (n - 1) fibs)

fib :: Int -> Integer
fib n = fibs !! n

--fib4 :: Int -> Integer
--fib4 n = index fibs n

largeFib1 :: Integer
largeFib1 = head (dropWhile (<= 1000) fibs)

my_repeat :: a -> [a]
my_repeat x = xs
	where xs = x : xs
	
data Tree a = Leaf
			| Node (Tree a) a (Tree a)

main :: IO ()
main = do
	
	let ans6 = take 20 fibs

	let ans7 = fib 4

	let ans8 = largeFib1

--	putStrLn (show ans6)
--	
--	putStrLn (show ans7)
--	
--	putStrLn (show ans8)

--	putStrLn (show ans8)
		
	putStrLn ("End of Main.")

