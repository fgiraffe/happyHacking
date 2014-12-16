#!/usr/bin/env runhaskell
{-# LANGUAGE NPlusKPatterns #-}

-- hw 9: types and classes

import Data.List
import Data.Char
import Unsafe.Coerce

----------------------------------------------------------------
-- Exercise 0

data Nat    =  Zero
            |  Succ Nat
            deriving Show

-- v1 - yes
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

-- v2 - YES
-- natToInteger (Succ n) = natToInteger n + 1
-- natToInteger Zero = 0

-- v 3 - NO
-- natToInteger n = natToInteger n 

-- v 4: YES
-- natToInteger (Succ n) = 1 + natToInteger n
-- natToInteger Zero = 0

-- v 5 NO
-- natToInteger Zero = 1
-- natToInteger (Succ n) = (1 + natToInteger n) - 1

-- v 6 YES
-- natToInteger = head .m
--     where   m Zero = [0]
--             m (Succ n) = [sum [x | x <- (1 : m n)]]

-- v 7 YES
-- natToInteger :: Nat -> Integer
-- natToInteger = \n -> genericLength [c | c <- show n, c == 'S']

-- v 8 NO does not type check
-- natToInteger :: Nat -> Integer
-- natToInteger = \n -> length [c | c <- show n, c == 'S']
----------------------------------------------------------------
exer0 :: IO ()
exer0 = do
    let myz = natToInteger Zero
    let allTest1 = [2,4,6,8]
    let myInt = 123
    let myNatThree = (Succ (Succ (Succ (Zero))))
    let myIntThree = natToInteger myNatThree
    let myNatTwelve = (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Zero)))))))))))))
    let myIntTwelve = natToInteger myNatTwelve

    putStrLn ("Zero in Nat: " ++ show Zero);
    putStrLn ("Zero converted to int: " ++ show (natToInteger Zero) ++ "\n");

    putStrLn ("Three in Nat: " ++ show myNatThree);
    putStrLn ("Three converted to int: " ++ show myIntThree ++ "\n");
    
    putStrLn ("Twelve in Nat: " ++ show myNatTwelve);
    putStrLn ("Three converted to int: " ++ show myIntTwelve ++ "\n");
    
    
----------------------------------------------------------------         
-- Ex 1

integerToNat :: Integer -> Nat
-- v 1: YES. got question wrong because I misread this one
integerToNat 0 = Zero
integerToNat (n + 1) = Succ (integerToNat n)

-- v 2 : NO infinite recursion
-- integerToNat 0 = Succ Zero
-- integerToNat n = (Succ (integerToNat n))

-- v 3 NO does NOT type check
-- integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]

-- v 4: NO again infinite recursion
-- integerToNat n = integerToNat n

-- v5 YES
-- integerToNat (n + 1) = Succ (integerToNat n)
-- integerToNat 0 = Zero

-- v6 YES
-- integerToNat (n + 1) = let m = integerToNat n in Succ m
-- integerToNat 0 = Zero

-- v 7 NO does not type check
-- integerToNat = head . m
--     where   {
--             ; m 0 = [0]
--             ; m (n + 1) = sum [ [x | x <- (1 : m n)]]
--             }

-- v 8 NO
--integerToNat :: Integer -> Nat
-- integerToNat = \n -> genericLength [c | c <- show n, isDigit c]

exer1 :: IO ()
exer1 = do
    let cvt0 = integerToNat 0
    let cvt2 = integerToNat 2
    let cvt5 = integerToNat 5
    let cvt13 = integerToNat 13

    putStrLn ("Zero converted to Nat: " ++ show cvt0 ++ "\n");
    putStrLn ("2 converted to Nat: " ++ show cvt2 ++ " and back " ++ (show (natToInteger cvt2)) ++ "\n");
    putStrLn ("5 converted to Nat: " ++ show cvt5 ++ " and back " ++ (show (natToInteger cvt5)) ++ "\n");

    
    putStrLn ("Completed Exercise 1 ");

----------------------------------------------------------------         
-- Ex 1
add :: Nat -> Nat -> Nat

-- my gusses without coding:
-- 1: yes
-- 2: yes
-- 3: no, zero obv wrong
-- 4: no, zero obv wrong
-- 5: no, zero obv wrong
-- 6: no, zero obv wrong
-- 7: yes
-- 8: yes 

-- v 1: YES
-- add Zero n = n
-- add (Succ m) n = Succ (add n m)

-- v 2: YES
-- add (Succ m) n = Succ (add n m)
-- add Zero n = n

-- v3 : obv wrong
-- add Zero n = Zero
-- add (Succ m) n = Succ (add m n)

-- v 4 obv wrong also

-- v 5 obv wrong just checking
-- add n Zero = Zero
-- add n (Succ m) = Succ (add n m)

-- v 6 wrong

-- v 7 YES
-- add n Zero = n
-- add n (Succ m) = Succ (add m n)

-- v 8 YES
add n (Succ m) = Succ (add m n)
add n Zero = n


exer2 :: IO ()
exer2 = do
    let n0 = integerToNat 0
    let n5 = integerToNat 5
    let n13 = integerToNat 13
    
    let zpz = add n0 n0
    let zp5 = add n0 n5
    let p5_0 = add n5 n0
    let p5_13 = add n13 n5

    putStrLn ("zero plus zero: " ++ show zpz ++ " and back " ++ (show (natToInteger zpz)) ++ "\n");
    putStrLn ("zero plus five: " ++ show zp5 ++ " and back " ++ (show (natToInteger zp5)) ++ "\n");
    putStrLn ("five plus zero: " ++ show p5_0 ++ " and back " ++ (show (natToInteger p5_0)) ++ "\n");
    putStrLn ("five plus thirteen: " ++ show p5_13 ++ " and back " ++ (show (natToInteger p5_13)) ++ "\n");
    
    
    
    putStrLn ("Completed Exercise 2");

----------------------------------------------------------------         
-- Ex 3 - mult

mult :: Nat -> Nat -> Nat

-- v 1 - no fails on 5 * 1
-- mult Zero Zero = Zero
-- mult m (Succ n) = add m (mult m n)

-- v2 YES
-- mult m Zero = Zero
-- mult m (Succ n) = add m (mult m n)

-- V 3 NOTE M N SWITCH, PEELING OFF WRONG NUM
-- mult m Zero = Zero
-- mult m (Succ n) = add n (mult m n)

-- V 4, NO INFINITE RECUSION
-- mult m Zero = Zero
-- mult m n = add m (mult m (Succ n))


exer3 :: IO ()
exer3 = do
    
    let n0 = integerToNat 0
    let n1 = integerToNat 1
    let n5 = integerToNat 5
    let n11 = integerToNat 11
    
    let t0_0 = mult n0 n0
    let t0_5 = mult n0 n5
    let t5_0 = mult n5 n0
    let t5_1 = mult n5 n1
    let t5_11 = mult n11 n5

    putStrLn ("zero * zero: " ++ show t0_0 ++ " and back " ++ (show (natToInteger t0_0)) ++ "\n");
    putStrLn ("zero * five: " ++ show t0_5 ++ " and back " ++ (show (natToInteger t0_5)) ++ "\n");
    putStrLn ("five * zero: " ++ show t5_0 ++ " and back " ++ (show (natToInteger t5_0)) ++ "\n");
    putStrLn ("five * one: " ++ show t5_1 ++ " and back " ++ (show (natToInteger t5_1)) ++ "\n");
    putStrLn ("five * thirteen: " ++ show t5_11 ++ " and back " ++ (show (natToInteger t5_11)) ++ "\n");
    
    
    putStrLn ("Completed Exercise 3");


----------------------------------------------------------------         

main :: IO ()
main = do
    -- exer0
    -- exer1
    -- exer2
    exer3
    
    putStrLn ("End of Homework 9.");
    
    
    
    
    
    
    
    
            