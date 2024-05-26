-- Function to generate all permutations of a list
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' (x:xs) = [front ++ [x] ++ back | ys <- permutations' xs, i <- [0..length ys], let (front, back) = splitAt i ys]

-- Main function to demonstrate the usage of permutations'
main :: IO ()
main = print (permutations' [1, 2, 3, 4])
