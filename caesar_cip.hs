#!/usr/bin/env runhaskell

import Data.Char

ref_table                         :: [Float]
ref_table                         =  [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
                                  6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
                                  7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
                                  1.0, 2.4, 0.2, 2.0,  0.1]


-- list of indicies that a item appears in a list
positions                     :: Eq a => a -> [a] -> [Int]
positions x xs                =  [i | (x',i) <- zip xs [0..], x == x']


-- count the lower case letters in a string


lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]


count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']


-- Cassar cipher example, only on lower case letters

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

uclet2int :: Char -> Int
uclet2int c = ord c - ord 'A'

int2uclet :: Int -> Char
int2uclet n = chr(ord 'A' + n)

-- takes a shift value and a char
-- if char is lower case returns shifted char, otherwise return char

shift :: Int -> Char -> Char
shift n c   | isLower c = int2let((let2int c + n) `mod` 26)
            | isUpper c = int2uclet((uclet2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


-- OK, now let's crack the cipher!

-- percent 5 6 -> 83.3333
percent :: Int -> Int -> Float
percent n m = (fromIntegral n) / (fromIntegral m) * 100


-- calc letter freqs for a given string
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']] where n = lowers xs


-- chisqr: compare observed frequencies with expected frequencies
-- smaller result from chisqr, the better the match

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o - e) ^ 2) / e | (o, e) <- zip os es]

-- rotate elements of a list to the left

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- now the solver

crack :: String -> String
crack xs = encode( - factor ) xs
    where
        factor = head( positions( minimum chitab ) chitab)
        chitab = [chisqr (rotate n table') ref_table | n <- [0..25]]
        table' = freqs xs


main :: IO ()
main = do

    let tpm_text = "With regards to prescription medicines, information about the formularies was not readily available, Reilly added. Patients need to have a clear idea of which medicines are covered and which are not. Respondents also had concerns with how the average private health insurance plan covers medicines. Three quarters of Americans believe health insurance should cover the same proportion or a larger proportion of prescription drug costs as the cost of hospital and physician services. Government studies have shown that those with private insurance typically pay for a larger share of their total prescription drug costs compared to other costs. The survey showed a need for more clarity and ease of understanding on what medicines the plans actually cover. An overwhelming majority of Americans believe consumers should have access to the list of medicines and out-of-pocket costs when selecting a health plan. Consumers deserve an out-of-pocket cost calculator, like exists for Medicare Part D, for easy comparison of expected costs, said Reilly. Clear links to formularies in 2015 would be an excellent start, especially for patients with chronic illnesses, the most frequ ent users of the health care system."

    let freq_tab = zip ['a'..'z'] (freqs tpm_text)

    putStrLn (show freq_tab)

