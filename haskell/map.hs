
double :: Int -> Int
double x = x * 2

myMap :: (a->b) -> [a] -> [b]
myMap _[] = []
myMap f (x:xs) = f x : myMap f xs


myList :: [Int]
myList = [1, 2, 3, 4, 5]


main :: IO ()
main = print(myMap double myList)
