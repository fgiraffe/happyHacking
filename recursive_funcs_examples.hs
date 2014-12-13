#!/usr/bin/env runhaskell

-- recursive_funcs_examples.hs
-- interesting examples and exercises from
-- Chapter 6 of Programming in Haskell: all about recursive functions

my_reverse :: [a] -> [a]

my_reverse [] = []
my_reverse (x : xs) = my_reverse( xs ) ++ [x]

-- define my own list append using symbol +-+

(+-+) :: [a] -> [a] -> [a]

[] +-+ xs       = xs
(x:xs) +-+ ys   = x : (xs +-+ ys)


-- insert into sorted list

insert_sorted :: Ord a => a -> [a] -> [a]

insert_sorted x [] = [x]
insert_sorted x (y : ys)    | x <= y    = x : y : ys
                            | otherwise = y : insert_sorted x ys


-- ZOMG this lets us do insertion sort

isort :: Ord a => [a] -> [a]

isort []        = []
isort (x:xs)    = insert_sorted x (isort xs)

-- Quicksort, from the lecture (no mutation, copying)

qsort :: [Int] -> [Int]

qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <-xs, b > x]

-- lecture example maybe also on homework
-- replicate

my_replicate_lc :: Int -> a -> [a]

my_replicate_lc 0 _ = []
my_replicate_lc n x = [x | _ <- [1..n]]


my_replicate_recursion :: Int -> a -> [a]

my_replicate_recursion 0 _ = []
my_replicate_recursion n x = x : (my_replicate_recursion (n - 1) x)


-- lecture example maybe also on homework
-- select_nth

select_nth_recursion :: [a] -> Int -> a
select_nth_recursion (x:xs) 0  = x
select_nth_recursion (x:xs) n = select_nth_recursion xs (n - 1)



-- is element in list?

elem_recursion :: Eq a => a -> [a] -> Bool

elem_recursion _ [] = False
elem_recursion elem (x:xs) = if elem == x
                                then True
                                else elem_recursion elem xs

my_r_drop :: Int -> [a] -> [a]

my_r_drop _ [] = []
my_r_drop 0 xs = xs
my_r_drop n (_:xs) = my_r_drop (n - 1) xs


-- my_init remove last element from a list


my_r_init :: [a] -> [a]

my_r_init [] = []
my_r_init (x : []) = []
my_r_init (x : xs) = x : my_r_init xs


-- better version from book

book_r_init :: [a] -> [a]

book_r_init [_] = []                    -- this is the one element list case!
book_r_init (x:xs) = x : book_r_init xs




-- exercises from Chapter 6

-- my exponential

(^^^) :: Int -> Int -> Int

x ^^^ 0 = 1
x ^^^ n = x * (x ^^^ (n - 1))

-- my_and
-- are all values in list True?

my_and :: [Bool] -> Bool

my_and [] = True
my_and (False : _) = False
my_and (True : xs) = my_and xs



-- my_concat
-- flatten a list of lists

my_concat :: [[a]] -> [a]

my_concat [] = []
my_concat (xs : xss) = xs ++ (my_concat xss)


-- (!!!) select nth element from list

(!!!) :: [a] -> Int -> a

(x : _) !!! 0 = x
(_ : xs) !!! n = xs !!! (n - 1)


-- merge - for merge sort

merge  :: Ord a => [a] -> [a] -> [a]

merge x [] = x
merge [] x = x
merge (x : xs) (y : ys) | x <= y    = x : merge xs (y : ys)
                        | otherwise = y : merge (x : xs) ys



halve :: [a] -> ([a], [a])
halve xs = do
    let first_chunk = (length xs) `div` 2
    ((take first_chunk xs), (drop first_chunk xs))



merge_sort :: Ord a => [a] -> [a]
merge_sort [] = []
merge_sort [a] = [a]

merge_sort xs = merge sorted_l sorted_r where
    (l, r) = halve xs
    sorted_l = merge_sort l
    sorted_r = merge_sort r

-- reference answer for msort
-- merge_sort xs = merge (merge_sort ys) (merge_sort zs) where
--     (ys, zs) = halve xs


main :: IO ()
main = do
    let n = 1
    let test_list = [0, 1,2,3,4,5]
    let evens = [0,2,4,6,8]
    let odds = [1,3,5,7]
    let mtlist = []

    let sort_test = [2,9,3,8,7,6,5,4,3,3,2,1,0]

    putStr "Merge Sorting "
    putStr (show sort_test)
    putStr " = "

    let result = merge_sort sort_test

    putStrLn ( show result )

































