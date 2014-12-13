#!/usr/bin/env runhaskell


putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs


main :: IO ()
main = do

    putStr' "ooo baby baby"
    putStrLn ( "Complete." )
