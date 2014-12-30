#!/usr/bin/env runhaskell

-- hw 13: foldl in terms of foldr

my_foldl1 :: (b -> a -> b) -> b -> [a] -> b
my_foldl1 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a 

my_foldl2 :: (b -> a -> b) -> b -> [a] -> b
my_foldl2 f a bs = foldr (\a b -> f b a) a bs  

my_foldl3 :: (b -> a -> b) -> b -> [a] -> b
my_foldl3 f = flip $ foldr (\a b g -> b (f g a)) id  

my_foldl4 :: (b -> a -> b) -> b -> [a] -> b
my_foldl4 = foldr . flip

fl_length_Func_1 = my_foldl1 (\n _ -> n + 1) 0
fl_length_Func_2 = my_foldl2 (\n _ -> n + 1) 0
fl_length_Func_3 = my_foldl3 (\n _ -> n + 1) 0
fl_length_Func_4 = my_foldl4 (\n _ -> n + 1) 0

fl_prod_Func_1 = my_foldl1 (*) 1
fl_prod_Func_2 = my_foldl2 (*) 1
fl_prod_Func_3 = my_foldl3 (*) 1
fl_prod_Func_4 = my_foldl4 (*) 1

-- took until THIS set of test funcs to find errors in 2 and 4.
-- so correct answer is 1 and 3

fl_reverse_Func_1 = my_foldl1 (\xs x -> x : xs) []
fl_reverse_Func_2 = my_foldl2 (\xs x -> x : xs) []
fl_reverse_Func_3 = my_foldl3 (\xs x -> x : xs) []
fl_reverse_Func_4 = my_foldl4 (\xs x -> x : xs) []



fl_Ref_Func_0 = foldl (\n _ -> n + 1) 0

fl_Ref_Func_1 = foldl (*) 1

fl_Ref_Func_2 = foldl (\xs x -> x : xs) []

main :: IO ()
main = do
    
    let empty_data = ""
    let test_data = "abcd"

    let ref_0 = fl_Ref_Func_2 empty_data
    let ref_1 = fl_Ref_Func_2 "abcd"

    let fl1_0 = fl_reverse_Func_1 empty_data
    let fl1_1 = fl_reverse_Func_1 test_data

    let fl2_0 = fl_reverse_Func_2 empty_data
    let fl2_1 = fl_reverse_Func_2 test_data

    let fl3_0 = fl_reverse_Func_3 empty_data
    let fl3_1 = fl_reverse_Func_3 test_data

    let fl4_0 = fl_reverse_Func_4 empty_data
    let fl4_1 = fl_reverse_Func_4 test_data
    

    putStrLn ( "Reference Answer, should be 0: " ++ show(ref_0) )
    putStrLn ( "Reference Answer, should be 5: " ++ show(ref_1) )

    putStrLn ( "" )
    putStrLn ( "Ans 1, should be " ++ (show ref_0) ++ ": " ++ show(fl1_0) )
    putStrLn ( "Ans 1, should be " ++ (show ref_1) ++ ": " ++ show(fl1_1) )

    putStrLn ( "" )
    putStrLn ( "Ans 2, should be " ++ (show ref_0) ++ ": " ++ show(fl2_0) )
    putStrLn ( "Ans 2, should be " ++ (show ref_1) ++ ": " ++ show(fl2_1) )

    putStrLn ( "" )
    putStrLn ( "Ans 3, should be " ++ (show ref_0) ++ ": " ++ show(fl3_0) )
    putStrLn ( "Ans 3, should be " ++ (show ref_1) ++ ": " ++ show(fl3_1) )

    putStrLn ( "" )
    putStrLn ( "Ans 4, should be " ++ (show ref_0) ++ ": " ++ show(fl4_0) )
    putStrLn ( "Ans 4, should be " ++ (show ref_1) ++ ": " ++ show(fl4_1) )

    putStrLn ( "\nHomework 13 Complete." )
