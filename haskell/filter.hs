myFilter :: (a-> Bool) -> [a] -> [a]
myFilter _[] = []
myFilter p (x:xs)
	| p x = x : myFilter  p xs
	| otherwise = myFilter p xs

isPrime ::Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..sqrtN]
    where sqrtN = floor (sqrt (fromIntegral n))

myList =[1,2,3,4,5,6,7,8,9]


main :: IO ()
main = print(myFilter isPrime myList)
