
-- 7
-- Удалить из исходного списка все повторные вхождения элементов

removeDuplicates = foldl (\res x -> if x `elem` res
                                      then res
                                      else res ++ [x]) []

---------------------------------------------------------------------------
-- 17
-- Определите функцию МНОЖЕСТВО, преобразующую список в множество.

makeSet = removeDuplicates
main = do
    putStrLn "Tests for task 7"
    putStrLn "Test 1"
    let list = [3,1,2,3,4,2,3,3,3]
    print (removeDuplicates list)

    putStrLn "Tests for task 17"
    putStrLn "Test 1"
    let list = [1,2,3,4,2,3]
    print (makeSet list)
    putStrLn "Test 2"
    let list = [1,2,3,4,2,3,1,7,8,1,2,4]
    print (makeSet list)
