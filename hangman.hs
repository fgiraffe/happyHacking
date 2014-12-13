#!/usr/bin/env runhaskell

import System.IO


diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]


getCh :: IO Char
getCh = do  hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

-- Note abobe: "c" is NOT a mutable variable,
-- it is just being bound to getChar

sgetLine :: IO String
sgetLine =
    do x <- getCh
       if x == '\n' then
        do putChar x
           return []
       else
           putChar '-'
           xs <- sgetLine
           return (x:xs)


guess :: String -> IO ()
guess word =
    do  putStr "> "
        xs <- getLine
        if xs == word then
            putStrLn "You win!"
        else
            do  putStrLn (diff word xs)
                guess word


hangman :: IO ()
hangman =
    do  putStrLn "Think of a word:"
        word <- sgetLine
        putStrLn "Guess the word"
        guess word


main :: IO ()
main = do
    putStrLn ("Hangman!")
