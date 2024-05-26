import Data.List (foldl')

-- Function to calculate the edit distance between two lists
editDistance :: Eq a => [a] -> [a] -> Int
editDistance xs ys = table !! length xs !! length ys
  where
    table = [[distance i j | j <- [0..length ys]] | i <- [0..length xs]]
    distance 0 j = j
    distance i 0 = i
    distance i j
      | xs !! (i-1) == ys !! (j-1) = table !! (i-1) !! (j-1)
      | otherwise = 1 + minimum [table !! (i-1) !! j, table !! i !! (j-1), table !! (i-1) !! (j-1)]

main :: IO ()
main = print (editDistance "hola" "palo")
