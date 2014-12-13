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

main :: IO ()
main = do
    let result = bin2int_listcomp [1, 0, 1, 1]
    putStr ("result from list comp: ")
    putStrLn (show result)

    let resultF = bin2int_fold [1, 0, 1, 1]
    putStr ("result from fold: ")
    putStrLn (show resultF)

    let resultI = make8 (int2bin 13)
    putStr ("result from int2bin: ")
    putStrLn (show resultI)

    let resultE = encode_str2b "abc"
    putStr ("result from encode_str2b: ")
    putStrLn (show resultE)

    putStr ("result from chop8: ")
    putStrLn (show (chop8 resultE))

    putStr ("result from encode_b2str: ")
    putStrLn (show (encode_b2str resultE))

    let srcString = "Rafa is sitting on the couch"
    putStr ("Testing: ")
    putStrLn srcString

    putStrLn (show (encode_str2b srcString))

    let trans = transmit srcString
    putStrLn (show trans)

