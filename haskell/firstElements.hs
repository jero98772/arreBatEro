firstElements :: [(a, b)] -> [a]
firstElements tuples = [x | (x, _) <- tuples]
list1 = [1, 5, 3, 9, 2]
list2 = [4, 7, 6, 8, 0]
main :: IO ()
main = print(firstElements [(1,4),(5,7),(3,6),(9,8),(2,0)])
