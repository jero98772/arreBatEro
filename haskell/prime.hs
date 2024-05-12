-- prime
isPrime ::Integer -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..sqrtN]
    where sqrtN = floor (sqrt (fromIntegral n))


main :: IO ()
main = print(isPrime 5)
