flatten :: [[a]] ->[a]
flatten [] =[]
flatten (x:xs) = x ++ flatten xs

main :: IO ()
main = do
	print(flatten [[1, 2, 3], [4, 5], [6, 7, 8]])
