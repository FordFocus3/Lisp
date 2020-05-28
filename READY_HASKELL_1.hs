import Data.List
-- 26 Задание
-- Реализовать алгоритм сортировки слиянием.

_mergeSort [] = []
_mergeSort [l] = [l]
_mergeSort l =
    _merge (_mergeSort firstHalf) (_mergeSort secondHalf) where
        firstHalf = take ((length l) `div` 2) l
        secondHalf = drop ((length l) `div` 2) l

_merge f [] = f
_merge [] s = s
_merge (f:ft) (s:st)
  | f < s     = f:(_merge ft (s:st))
  | otherwise = s:(_merge (f:ft) st)
  
  -- Задание 3.1
  -- Реализовать генератор деревьев, чтобы выдаваемые им деревья имели количество вершин,
  точно соответствующее числу, указанному в его первом аргументе.
  
  _test _in _out = do
    putStr "  -> "
    print _in
    putStr "  <- "
    print _out

main = do
    putStrLn "Test #1"
    let someList = [1,2,3,4,5]
    _test someList (_last someList)

    putStrLn "Test #3"
    let someList = [-3,-2,-1,0,1,2,3]
    _test someList (_split someList)

    putStrLn "Test #19.1"
    let someSmall = [1,2,3]
    let someBig = [5,3,4,1,2,6]
    let otherBig = [2,3,4]
    _test [someSmall, someBig] (_subset someSmall someBig)
    _test [someSmall, otherBig] (_subset someSmall otherBig)

    putStrLn "Test #19.2"
    let someSmall = [1,2,3]
    let someBig = [1,2,3]
    let otherBig = [1,2,3,4]
    _test [someSmall, someBig] (_selfSubset someSmall someBig)
    _test [someSmall, otherBig] (_selfSubset someSmall otherBig)

    putStrLn "Test #26"
    let someList = [5,1,4,3,2]
    _test someList (_mergeSort someList)

    putStrLn "Test #2.1"
    let n = 3
    _test n (_grayCode n)
