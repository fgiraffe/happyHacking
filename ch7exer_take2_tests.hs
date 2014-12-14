#!/usr/bin/env runhaskell

module Main where

import Test.HUnit

import Ch7exer_take2

t_range = [1..10]

plusOne = (+ 1)

---------------------------------------------------------------------
-- Exercise 1: change an expression from list comp 
-- to use map and filter

exer1_tests :: IO ()
exer1_tests = do
    let {
        mfh_t_ref_ans = [ plusOne x | x <- t_range, even x];
        ex1_ans1 = [ plusOne x | x <- t_range, even x];

        mfh_test1 = TestCase $ assertEqual 
                            "mf_higher called on [] should result in []"  
                            [] 
                            (mf_higher (+ 1) even []);
                    
        mfh_test2 = TestCase $ assertEqual 
                            "mf_higher called with +1 evens t_range" 
                            mfh_t_ref_ans
                            (mf_higher plusOne even t_range);

        mfh_tests = TestList [TestLabel "mf_higher tests" mfh_test1, mfh_test2];
    } in
        runTestTT mfh_tests

    putStrLn "My Ch7exer_take2 Exercise 1 Tests complete."

---------------------------------------------------------------------
-- Exercise 2: implement all, any, takeWhile, dropWhile 

exer2_tests :: IO ()
exer2_tests = do
    let {
        any_test_range = [1..3];
        testBoolList1 = [True, True, True, True];
        testBoolList2 = [False, True, True, True, True];
        testBoolList3 = [True, True, False, True, True];
        testBoolList4 = [True, True, False, True, True, False];

        ex2_all_ans1 = [all (&& True) testBoolList1, all (&& True) testBoolList2, 
                        all (&& True) testBoolList3, all (&& True) testBoolList4];
        ex2_my_all_ans1 = [my_all (&& True) testBoolList1, my_all (&& True) testBoolList2, 
                        my_all (&& True) testBoolList3, my_all (&& True) testBoolList4];

        my_all_test1 = TestCase $ assertEqual 
                            "my_all test 1"  ex2_all_ans1 ex2_my_all_ans1;

        my_all_test2 = TestCase $ assertEqual 
                            "my_all test 2 with empty" 
                            (all even [1..10]) (my_all even [1..10]);
        
        my_all_tests = TestList [TestLabel "my_all tests" my_all_test1, my_all_test2];
    
        -- my_any tests
        my_any_test0 = TestCase $ assertEqual 
                        "my_any test 0" 
                        (any even []) 
                        (my_any even []);
                        
        my_any_test1 = TestCase $ assertEqual 
                        "my_any test 1"  
                        (any even any_test_range) 
                        (my_any even any_test_range);

        my_any_tests = TestList [TestLabel "my_any tests" my_any_test0, my_any_test1];


        -- my_takeWhile tests
        tw_test_func = (> 3);
        
        my_tw_test0 = TestCase $ assertEqual "my_takeWhile test 0"  
                        (takeWhile tw_test_func []) 
                        (my_takeWhile tw_test_func []);
                        
        my_tw_test1 = TestCase $ assertEqual "my_takeWhile test 1" 
                        (takeWhile tw_test_func t_range) 
                        (my_takeWhile tw_test_func t_range);

        my_takeWhile_tests = TestList [TestLabel "my_takeWhile tests" my_tw_test0, my_tw_test1];
    
        -- my_dropWhile tests
        dw_test_func = (< 5);
        dw_test_range = [1..50];
        my_dw_test0 = TestCase $ assertEqual "my_dropWhile test 0" 
                                (dropWhile dw_test_func []) 
                                (my_dropWhile dw_test_func []);

        my_dw_test1 = TestCase $ assertEqual "my_dropWhile test 1" 
                                (dropWhile dw_test_func dw_test_range) 
                                (my_dropWhile dw_test_func dw_test_range);
        
        my_dropWhileTests = TestList [TestLabel "my_dropWhile tests" my_dw_test0, my_dw_test1];
    
    } in do
        runTestTT my_all_tests
        runTestTT my_any_tests
        runTestTT my_takeWhile_tests
        runTestTT my_dropWhileTests
    
    putStrLn "My Ch7exer_take2 Exercise 2 Tests complete."

---
exer3_tests :: IO ()
exer3_tests =
    let 
        map_test_range = [0..100];
        map_test_range2 = ["abc", "def", "ghi"];
        
        map_test_func = (+ 10);
        filter_test_range = [0..20];

        my_map_test1 = TestCase $ assertEqual "map test 1"  
                                (map map_test_func map_test_range)
                                (my_map map_test_func map_test_range);

        my_filter_tests = TestList [TestLabel "my_filter tests" my_filter_test_0, my_filter_test_1];

        my_map_test0 = TestCase $ assertEqual "map test 2 empty list"  
                                (map map_test_func [])
                                (my_map map_test_func []);

        my_map_test3 = TestCase $ assertEqual "map test 3 with reverse"
                                (map reverse map_test_range2)
                                (my_map reverse map_test_range2);
                                
        my_map_tests = TestList [TestLabel "my_map tests" my_map_test1, my_map_test0];
        
        
        my_filter_test_0 = TestCase $ assertEqual "filter empty list case"
                                    (filter even [])
                                    (my_filter even []);
        
        my_filter_test_1 = TestCase $ assertEqual "filter empty evens"
                                    (filter even map_test_range)
                                    (my_filter even map_test_range);
        
    
    in do 
        runTestTT my_map_tests
        runTestTT my_filter_tests
        putStrLn "My Ch7exer_take2 Exercise 3 Tests complete."
    
    
    
main :: IO ()
main = 
    do
    exer1_tests
    exer2_tests
    exer3_tests
    putStrLn "All Ch7exer_take2 Tests complete."
















