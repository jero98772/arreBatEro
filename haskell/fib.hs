fibonacci :: Integer -> Integer
fibonacci n = fibMemo !! fromIntegral n
  where
    fibMemo = map fib [0 ..]

    fib 0 = 0
    fib 1 = 1
    fib m = fibMemo !! (m - 1) + fibMemo !! (m - 2)

main :: IO ()
main = print(fibonacci 50000)
