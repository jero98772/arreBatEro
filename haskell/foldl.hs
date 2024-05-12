myFold :: (b -> a -> b) -> b -> [a] -> b
myFold _ acc [] = acc
myFold f acc (x:xs) = myFold f (f acc x ) xs

sumList :: [Int] -> Int
sumList = myFold (+) 0

main :: IO ()
main = print(sumList [1, 2, 3, 4, 5])
