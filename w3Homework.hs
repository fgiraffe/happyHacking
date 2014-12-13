import Prelude hiding ((&&))
-- import Prelude hiding ((||))

simplist = [1,2,3,4,5]
shortlist = [50,60,70]
zerotonine = [0,1,2,3,4,5,6,7,8,9]
zerototen = [0,1,2,3,4,5,6,7,8,9,10]
listolists = [[1,2], [3,4,5], [6,7,8,9]]

-- Q0

-- halve :: [a] -> ([a],[a])
-- halve xs =
--     let
--         len = length xs
--         halflength = len `div` 2
--         even = if (len `rem` 2) == 0 then True else False
--     in
--         if even
--         then (take halflength xs, drop halflength xs)
--         else ([],[])

-- hv1 xs = (take n xs, drop n xs)
--     where n = length xs / 2
-- that "/" wont work

-- works
hv3 xs = splitAt (length xs `div` 2) xs

hv4 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs

-- hv5 xs = splitAt (length xs `div` 2)
-- not enough params to splitAt

hv6 xs = (take n xs, drop (n + 1) xs)
    where n = length xs `div` 2
-- BROKEN skips hd of second pair

hv7 xs = splitAt (div (length xs) 2) xs

-- h8 xs = splitAt (length xs / 2) xs
-- again the / is bad

-- Q1

safetail_cond :: [a] -> [a]
safetail_cond xs = if null xs
                    then []
                    else drop 1 xs

safetail_guard :: [a] -> [a]
safetail_guard xs | null xs == True = []
                  | otherwise = drop 1 xs

safetail_patt :: [a] -> [a]
safetail_patt [] = []
safetail_patt (_:xs) = xs

-- st1 :: [a] -> [a]
st1 xs = if null xs then [] else tail xs

st2 [] = []
st2 (_:xs) = xs

-- st3 fails on [] case, non exhaustive list
st3 (_:xs)
    | null xs = []
    | otherwise = tail xs

st4 xs
    | null xs = []
    | otherwise = tail xs

-- overlapping patterns
-- st5 xs = tail xs
-- st5 [] = []

-- st6 [] = []
-- st6 xs = tail xs

-- [] throws exception
-- st7 [x] = [x]
-- st7 (_:xs) = xs

-- st8
--     = \ xs ->
--         case xs of
--             [] -> []
--             (_:xs) -> xs


-- Q2

-- False || False = False
-- _ || _ = True
--

-- False || b = b
-- True || _ = True

-- b || c
--     | b == c = b
--     | otherwise = False
--

-- b || c
--     | b == c = b
--     | otherwise = True

-- b || False = b
-- _ || True = True


-- b || c
--     | b == c = c
--     | otherwise = True

-- b || True = b
-- _ || True = True
-- FAIL non exhaustive pattern

-- False || False = False
-- False || True = True
-- True || False = True
-- True || True = True



-- Q3

-- True && True = True
-- _ && _ = False
-- Good

-- a && b = if a then if b then True else False else False
-- Good

-- a && b = if not (a) then not (b) else True
-- FAIL

-- a && b = if a then b
-- FAIL if syntex

-- a && b = if a then if b then False else True else False
-- FAIL

-- a && b = if a then b else False
-- GOOD

-- a && b = if b then a else False
-- GOOD

-- test_func = (True && True == True, True && False == False, False && True == False, False && False == False)

-- Q4

m0 x y z = x * y * z
-- m0 :: Num a => a -> a -> a -> a

m1 x y z = \ x -> (\ y -> (\ z -> x * y * z))
-- m1 :: Num a => t -> t1 -> t2 -> a -> a -> a -> a

--m2 = \ x -> (x * \ y -> (y * \ z -> z))

-- mylam_mult = \x -> (\y -> (\z -> x * y * z))

-- mylam2_mult = ((( (\x -> \y) -> \z) -> x * y) * z)


-- Q7
-- which takes a number n and a list and
-- removes the element at position n from the list.
-- Ex: remove 0 [1 2 3 4] = [2,3,4]

-- remove :: Int -> [a] -> [a]

r1 n xs = take n xs ++ drop n xs
-- no, returns whole list

r2 n xs = drop n xs ++ take n xs
-- nope, shuffles the list

r3 n xs = take (n + 1) xs ++ drop n xs
-- dupes nth element

r4 n xs = take n xs ++ drop (n + 1) xs
-- WINNER winner chicken dinner

{-
    Lab section
-}



e0 = [False, True, False, True]

e1 = [[1,2], [3,4]]

e2 = [[[1, 2, 3]], [[3, 4, 5]]]
-- [[[Integer]]]

e6 x y = x * y
-- Num a => a -> a -> a

e9 [x, y] = (x, True)
-- [t] -> (t, Bool)

e10 (x, y) = [x, y]
-- (t, t) -> [t]

e12 = ('\a', True)



e132 :: Int -> Int -> Int

e131 x y = x / y
-- Num a => a

e132 x y = x + y * y
-- Num a => a

e133 x = x * x
-- Num a => a

e134 (x, y) = x * y

e141 = [('C', 0.1)]
-- [(Char, Double)]

e142 = (['c', 'u', 'r', 'r', 'y'], 0.0)
-- ([Char], Double)

e143 = (["Haskell"], [1.1, 2.2, 3.3])
-- ([[Char]], [Double])

e144 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])
-- ([Char], [Double])

e14 = (['c', 'u', 'r', 'r', 'y'], 0.0)




