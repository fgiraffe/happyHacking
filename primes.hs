#!/usr/bin/env runhaskell

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]


main :: IO ()
main = do
	let n = 10000
	let firstN = take n primes
	let nextToLast = firstN !! (n - 1)
	putStrLn ("The " ++ show n ++ "th prime is: " ++ show nextToLast)

