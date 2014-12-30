#!/usr/bin/env runhaskell

-- type Parser a = String -> [(a, String)]
--
-- -- always returns v
-- return_p    ::    a -> Parser a
-- return_p v    =    \inp -> [(v, inp)]
--
-- -- always fails
-- failure        :: Parser a
-- failure        = \inp -> []
--
-- -- item: [] for [],m otherwise first item
-- item        :: Parser Char
-- item        = \inp -> case inp of
--                     [] -> []
--                     (x : xs) -> [(x, xs)]
--
-- -- parse application function
-- parse        :: Parser a -> String -> [(a, String)]
-- parse p inp = p inp


-- example parser that consumes 3 chars, skips the second char, returns 1st and 3rd
example_parser	:: Parser(Char, Char)
example_parser = do x <- item;
					y <- item;
					return_p (x,y)

main :: IO ()
main = do

	putStrLn ("End of Main.")
