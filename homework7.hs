#!/usr/bin/env runhaskell

-- Ex 1

my_all :: (a -> Bool) -> [a] -> Bool

-- 1: YES
-- my_all p xs = and (map p xs)

-- 2: NO - no parse
-- my_all p xs = map p (and xs)

-- 3: YES
-- my_all p = and . map p

-- 4: YES
-- my_all p = not . any (not . p)

-- 5: NO
-- my_all p = map p . and

-- 6: YES
-- my_all p xs = foldl (&&) True (map p xs)

-- 7: NO
-- my_all p xs = foldr (&&) False (map p xs)

-- 8: YES
my_all p = foldr (&&) True . map p

-- ###########################################

-- Ex 2

my_any :: (a -> Bool) -> [a] -> Bool

-- 1: NO - wont type check
-- my_any p = map p . or

-- 2: YES
-- my_any p = or . map p

-- 3: YES
-- my_any p xs = length (filter p xs) > 0

-- 4: YES
-- my_any p = not . null . dropWhile (not . p)

-- 5: NO
-- my_any p = null . filter p

-- 6: YES
-- my_any p xs = not (all (\ x -> not (p x)) xs)

-- 7: YES
my_any p xs = foldr (\ x acc -> (p x) || acc) False xs

-- 8: I said YES, but that is WRONG. Testing missed it
-- fails on empty list!
-- my_any p xs = foldr (||) True (map p xs)

-- ###########################################

-- Ex 5
my_map :: (a -> b) -> [a] -> [b]

-- my_map f = foldr(\ x xs -> xs ++ [f x]) []
-- nope, reverses the input list

-- my_map f = foldr(\ x xs -> f x ++ xs) []
-- nope does not type check

-- my_map f = foldl (\ xs x -> f x : xs) []
-- nope again backwards

-- my_map f = foldl (\ xs x -> xs ++ [f x]) []

my_map f = foldr (\ x xs -> [f x] ++ xs) []

-- ###########################################

-- Ex 6
-- my filter

my_filter :: (a -> Bool) -> [a] -> [a]

-- 1 : nope: reverses outputs
-- my_filter p = foldl (\ xs x -> if p x then x : xs else xs) []

my_filter p = foldr (\x xs -> if p x then x : xs else xs) []


-- ###########################################

-- Ex 7
dec2int :: [Integer] -> Integer

-- 1 : no
-- dec2int = foldr (\x y -> 10 * x + y) 0

-- 2 no
-- dec2int = foldl (\ x y -> x + 10 * y) 0

-- 3 :yes
dec2int = foldl (\x y -> 10 * x + y) 0

-- 4: nope
--dec2int = foldr( \x y -> x + 10 * y) 0

-- ###########################################

-- Ex 8
-- compose :: [a -> a] -> (a -> a)
-- compose = foldr (.) id
--
-- sumsqeven = compose [sum, map (^ 2), filter even]


-- ###########################################

-- Ex 9
-- my curry

my_test_fun :: Num a => (a, a) -> a
my_test_fun(x, y) = x + y

my_test_cur_fun :: Num a => a -> (a -> a)
my_test_cur_fun x y = x + y


my_curry :: ((a, b) -> c) -> a -> b -> c
-- 1: no, wont type check
-- my_curry f = \ x y -> f x y

-- 2: no, wont type check
--my_curry f = \x y -> f

-- 3 : yes? wtf?
my_curry f = \ x y -> f (x, y)

-- 4: no
-- my_curry f = \ (x, y) -> f x y

-- ###########################################

-- Ex 10
-- my UN curry

my_uncurry :: (a -> b -> c) -> (a, b) -> c
my_uncurry f = \ (x, y) -> f x y


-- ###########################################
-- Ex 11

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]

unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)



ref_chop8 :: [Int] -> [[Int]]
ref_chop8 [] = []
ref_chop8 bits = take 8 bits : ref_chop8 (drop 8 bits)

fold_chop8 :: [Int] -> [[Int]]

fold_chop8 = unfold null (take 8) (drop 8)


-- ###########################################
-- Ex 12

uf_map :: (a -> b) -> [a] -> [b]
-- 1: no type check
-- uf_map f = unfold null (f) tail

-- 2: no type check
-- uf_map f = unfold null (f (head)) tail

uf_map f = unfold null (f . head) tail


-- ###########################################
-- Ex 12

uf_iterate :: (a -> a) -> a -> [a]

-- take 10 (uf_iterate (+1) 1)

-- 1 : yes?
uf_iterate f = unfold (const False) id f

-- 2 : no, returns [2,3,4,5,6,7,8,9,10,11]
-- uf_iterate f = unfold (const False) f f

-- 3 no, result is empty list []
-- uf_iterate f = unfold (const True) id f

-- 4 no, result is empty list []
-- uf_iterate f = unfold (const True) f f





-- ###########################################
-- ###########################################

main :: IO ()
main = do

    let allTest1 = [2,4,6,8]
    let allTest2 = [1,2,4,6,8]
    let allTest3 = [1,2,4,6,8,9]

    putStrLn ("Q7 dc 2 int")
    putStrLn ("Q7 dec2int [2, 3, 4, 5]:" ++ (show (dec2int [2, 3, 4, 5])))
    putStrLn ("Q7 dec2int []:" ++ (show (dec2int [])))
    putStrLn ("Q7 dec2int [0,0,0,0]:" ++ (show (dec2int [])))


    putStrLn ("----------")
    let myFILTa1 = my_filter odd allTest3
    let refFILTa1 = filter odd allTest3

    putStrLn ("Q6 my filter")
    putStrLn ("Q6 My  answ:" ++ (show myFILTa1))
    putStrLn ("Q6 My  answ:" ++ (show refFILTa1))

    let myFILTa2 = my_filter odd []
    let refFILTa2 = filter odd []
    putStrLn ("Q6 My  answ:" ++ (show myFILTa2))
    putStrLn ("Q6 My  answ:" ++ (show refFILTa2))

    putStrLn ("----------")

    let myMAPa1 = my_map (+ 1) [1,2,3]
    let refMAPa1 = map (+1) [1,2,3]
    putStrLn ("Q5 my map")
    putStrLn ("Q5 My  answ:" ++ (show myMAPa1))
    putStrLn ("Q5 My  answ:" ++ (show refMAPa1))

    putStrLn ("----------")



    let myAllAnsw1 = my_all even allTest1
    let refAllAnsw1 = all even allTest1
    putStrLn ("Q1 My  all")
    putStrLn ("Q1 My  answ:" ++ (show myAllAnsw1))
    putStrLn ("Q1 Ref answ:" ++ (show refAllAnsw1))

    let myAllAnsw2 = my_all even allTest2
    let refAllAnsw2 = all even allTest2

    putStrLn ("Q1 My  answ:" ++ (show myAllAnsw2))
    putStrLn ("Q1 Ref answ:" ++ (show refAllAnsw2))

    let myAllAnsw3 = my_all even allTest3
    let refAllAnsw3 = all even allTest3

    putStrLn ("Q1 My  answ:" ++ (show myAllAnsw3))
    putStrLn ("Q1 Ref answ:" ++ (show refAllAnsw3))

    putStrLn ("\nQ2 My any")
    let myAnyAnsw1 = my_any even allTest1
    let refAnyAnsw1 = any even allTest1
    putStrLn ("Q2 My  answ:" ++ (show myAnyAnsw1))
    putStrLn ("Q2 Ref answ:" ++ (show refAnyAnsw1))


    let myAnyAnsw2 = my_any odd []
    let refAnyAnsw2 = any odd []
    putStrLn ("Q2 My  answ:" ++ (show myAnyAnsw2))
    putStrLn ("Q2 Ref answ:" ++ (show refAnyAnsw2))




