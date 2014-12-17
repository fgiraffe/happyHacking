#!/usr/bin/env runhaskell

-- String Transmitter example from "Higher Order Functions" chapter 7

--import Language.Haskell.TH
import Data.Char


type Bit = Int

bin2int_listcomp :: [Bit] -> Int
bin2int_listcomp bits = sum [w * b | (w, b) <- zip weights bits]
                        where weights = iterate (*2) 1

bin2int_fold :: [Bit] -> Int
bin2int_fold = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- pad to 8 bits
make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- encode to binary

encode_str2b :: String -> [Bit]
encode_str2b = concat . map (make8 . int2bin .ord)

-- parity additions
parity :: [Bit] -> Int
parity [] = 0
parity xs = (rem (length (filter (== 1) xs)) 2)

parity_check :: [Bit] -> Bool
parity_check [] = True
parity_check xs = (parity (tail xs)) == (head xs)

remove_parity :: [Bit] -> [Bit]
remove_parity [] = []
remove_parity xs = tail xs

verify9 :: [[Bit]] -> [[Bit]]
verify9 [] = []
verify9 (x:xs) = 
    if parity_check x then
        x : verify9 xs
    else
        error "Chunk failed validation"

-- pad to 9 bits: 8 real bits + 1 parity bit
make9 :: [Bit] -> [Bit]
make9 bits = 
    let 
        padded_to_8 = make8 bits
    in
        parity padded_to_8 : padded_to_8


chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

encode_str2b_parity :: String -> [Bit]
encode_str2b_parity = concat . map (make9 . int2bin . ord)

encode_b2str_parity :: [Bit] -> String
encode_b2str_parity = map(chr . bin2int_fold) . (map remove_parity) . verify9 . chop9


-- now go back from bit list to Str

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

encode_b2str :: [Bit] -> String
encode_b2str = map(chr . bin2int_fold) . chop8

-- transmit a stream of characters as a bit list

transmit :: String -> String
transmit = encode_b2str . channel . encode_str2b

channel :: [Bit] -> [Bit]
channel = id

test_parity :: IO ()
test_parity = do
    -- let up_to_1000 = [1..1000]
    --
    --
    -- --lowers :: String -> Int
    -- lowers xs = length [x | x <- xs, isLower x]
    
    
    putStrLn ("parity Tests complete.")
    
misc_test_parity :: IO ()
misc_test_parity = do
    let ptl_0_0 = []
    let ptl_0_1 = [0]
    let ptl_2 = [1]
    let ptl_0_3 = [0,0]
    let ptl_4a = [1,0]
    let ptl_4b = [0,1]
    let ptl_0_5a = [1,0,1]
    let ptl_5b = [1,0,0]
    let ptl_5c = [0,0,1]
    let ptl_0_5d = [0,1,1]
    let ptl_5e = [1,1,1]
    
    putStr "Parity of "
--    putStr (show [1,1,1])
    putStrLn (show (parity ptl_0_0))
    putStrLn (show (parity ptl_0_1))
    putStrLn (show (parity ptl_0_3))
    putStrLn (show (parity ptl_0_5a))
    putStrLn (show (parity ptl_0_5d))
    putStrLn (show (parity ptl_2))
    putStrLn (show (parity ptl_5b))
    putStrLn (show (parity ptl_5c))
    putStrLn (show (parity ptl_5e))
    
    

main :: IO ()
main = do
    -- let result = bin2int_listcomp [1, 0, 1, 1]
    -- putStr ("result from list comp: ")
    -- putStrLn (show result)
    --
    -- let resultF = bin2int_fold [1, 0, 1, 1]
    -- putStr ("result from fold: ")
    -- putStrLn (show resultF)
    --
    -- let resultI = make8 (int2bin 13)
    -- putStr ("result from int2bin: ")
    -- putStrLn (show resultI)
    --
    -- let resultE = encode_str2b "abc"
    -- putStr ("result from encode_str2b: ")
    -- putStrLn (show resultE)
    --
    -- putStr ("result from chop8: ")
    -- putStrLn (show (chop8 resultE))
    --
    -- putStr ("result from encode_b2str: ")
    -- putStrLn (show (encode_b2str resultE))

    -- misc_test_parity

    let srcString = "Rafa is sitting on the couch"
    putStr ("Testing: ")
    putStrLn srcString

    putStrLn (show (encode_str2b srcString))

    let trans = transmit srcString
    putStr "\n"
    putStrLn (show trans)
    
    putStrLn "\nNow with parity"
    let encodedWParity = encode_str2b_parity srcString
    putStrLn (show encodedWParity)

    let decodedWParity = encode_b2str_parity encodedWParity
    putStrLn (show decodedWParity)
    
    
    
    putStrLn "End of encode/decode tests."
    
    

