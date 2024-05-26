maxMin :: Ord a => [a] -> [a] -> (a,a)
maxMin xs ys= (maximum xs, minimum ys)
list1 = [1, 5, 3, 9, 2]
list2 = [4, 7, 6, 8, 0]
main :: IO ()
main = print(maxMin list1 list2)
