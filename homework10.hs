#!/usr/bin/env runhaskell

-- Homework 10 : countdown problem

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
				where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] 	= [[x]]
interleave x (y : ys) = (x : y : ys): map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] 		= [[]]
perms (x : xs) 	= concat (map (interleave x) (perms xs))

book_choices :: [a] -> [[a]]
book_choices xs = concat (map perms (subs xs))

------------------------------------------------------------
-- ex 0

choices :: [a] -> [[a]]

-- opt 1 ? maybe? No outputs dupes
-- choices xs = [ys ++ zs | ys <- subs xs, zs <- perms ys]

-- opt 2 does not typecheck
-- choices xs = concat [zs | ys <- subs xs, zs <- perms ys]

-- opt 3: YES
choices xs = [zs | ys <- subs xs, zs <- perms ys]

------------------------------------------------------------
-- ex 1, guessed correctly

removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y : ys)	
		| x == y = ys
		| otherwise = y : removeone x ys

------------------------------------------------------------
-- ex 2
isChoice :: Eq a => [a] -> [a] -> Bool

-- opt 1
isChoice [] _ = True
isChoice (x : xs) [] = False
isChoice (x: xs) ys = elem x ys && isChoice xs (removeone x ys)

exer2 :: IO ()
exer2 = do
    
    let one2five = [1..5]
    let one2ten = [1..10]
    
    putStrLn ("should be true: " ++ show (isChoice one2five one2ten));

    putStrLn ("should be true: " ++ show (isChoice one2ten one2ten));
    putStrLn ("should be true: " ++ show (isChoice [] one2ten));
    
    putStrLn ("should be false: " ++ show (isChoice one2ten one2five));
    putStrLn ("should be false: " ++ show (isChoice one2ten []));
    
    putStrLn ("End of Exercise 2 tests.");

------------------------------------------------------------
-- ex 3: split
split :: [a] -> [([a],[a])]

-- opt 1 - does not typecheck
-- split [] = []
-- split [x] = [x]
-- split (x : xs) = [([x] : (ls ++ rs)) | (ls, rs) <- split xs]

-- opt 2 - also no typecheck
-- split [] = []
-- split (x : xs) = ([x] : xs) : (split xs)


-- opt 3: typechecks, but returns []
-- split [] = []
-- split (x : xs) = [(x : ls, rs) | (ls, rs) <- split xs]


-- opt 4: typechecks, but returns []
split [] = []
split [_] = []
split (x : xs)  = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exer3 :: IO ()
exer3 = do
    
    let one2four = [1..4]
    
    putStrLn ("\nSplit " ++ (show one2four) ++ " : " ++ show (split one2four));
    putStrLn ("\nSplit " ++ (show [1]) ++ " : " ++ show (split [1])); -- emoits [] not sure it should???

    putStrLn ("\nSplit " ++ (show [1,2]) ++ " : " ++ show (split [1,2])); -- emoits [] not sure it should???
    
    putStrLn ("\nEnd of Exercise 3 tests.");
    

main :: IO ()
main = do
    
    exer2
    exer3
    
    putStrLn ("End of Homework 10.");
    
    
    
    