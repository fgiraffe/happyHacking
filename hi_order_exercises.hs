#!/usr/bin/env runhaskell

import Data.Char

-- Ex 1: do list comp [f x | x <- xs, p x]
-- using map and filter

f :: Int -> Int
f x = x * 2

p :: Int -> Bool
p x = even x


-- Ex 2: all any takeWhile dropWhile
-- did this totally wrong on first try, should have checked answer

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll fn (x:xs) = (fn x) && (myAll fn xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny fn (x:xs) = (fn x) || (myAny fn xs)


-- cribbed from lecture

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile fn [] = []
myTakeWhile fn (x:xs)   | fn x      = x : myTakeWhile fn xs
                        | otherwise = []


myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile fn [] = []
myDropWhile fn (x:xs)   | fn x      = myDropWhile fn xs
                        | otherwise = x : xs


-- implement map f and filter p using foldr

-- myMap :: (a -> b) -> [a] -> [b]
-- myMap _ [] = []
-- myMap fn xs = foldr []




main :: IO ()
main = do
    putStrLn ("Chapter 7: Higher order functions:")

    let src_list = [0 .. 10]

    -- Q 1
    putStrLn "Q1 ---------------------"
    let answ_lc = [f x | x <- src_list, p x]
    putStrLn ( show answ_lc )

    let answ_ho = map f (filter p src_list)
    putStrLn ( show answ_ho )

    --
    putStrLn ""
    putStrLn "My Any tests---------------------"
    let myAllAnsw = myAll p answ_ho
    let refAllAnsw = all p answ_ho

    putStrLn ("My  answ:" ++ (show myAllAnsw))
    putStrLn ("Ref answ:" ++ (show refAllAnsw))

    let bad_list = [0,2,4,5,8,10]
    let myAllAnsw2 = myAll p bad_list
    let refAllAnsw2 = all p bad_list

    putStrLn ("My  answ:" ++ (show myAllAnsw2))
    putStrLn ("Ref answ:" ++ (show refAllAnsw2))

    putStrLn ""
    putStrLn "My Any tests---------------------"

    let oneEvenNum = [1,3,5,6,9]
    let noEvenNum = [1,3,5,7,9]
    let myAnyAnsw = myAny p oneEvenNum
    let refAnyAnsw = any p oneEvenNum

    putStrLn ("My  answ:" ++ (show myAnyAnsw))
    putStrLn ("Ref answ:" ++ (show refAnyAnsw))

    let myAnyAnsw2 = myAny p noEvenNum
    let refAnyAnsw2 = any p noEvenNum

    let answ3 = myAny p bad_list
    putStrLn ("My  answ:" ++ (show myAnyAnsw2))
    putStrLn ("Ref answ:" ++ (show refAnyAnsw2))


    putStrLn ""
    putStrLn "My takeWhile tests---------------------"
    let mtwAnsw = myTakeWhile (< 5) oneEvenNum
    let reftwAnsw = takeWhile (< 5) oneEvenNum

    putStrLn ("My  answ:" ++ (show mtwAnsw))
    putStrLn ("Ref answ:" ++ (show reftwAnsw))

    let mtwAnsw2 = myTakeWhile (< 0) oneEvenNum
    let reftwAnsw2 = takeWhile (< 0) oneEvenNum

    putStrLn ("My  answ:" ++ (show mtwAnsw2))
    putStrLn ("Ref answ:" ++ (show reftwAnsw2))


    putStrLn ""
    putStrLn "My dropWhile tests---------------------"
    let mdwAnsw = myDropWhile (< 5) oneEvenNum
    let refdwAnsw = dropWhile (< 5) oneEvenNum
    putStrLn ("My  answ:" ++ (show mdwAnsw))
    putStrLn ("Ref answ:" ++ (show refdwAnsw))

