absValue :: Int -> Int
absValue n
  | n<0 = n * (-1)
  | otherwise = n


power :: Int -> Int -> Int
power _ 0 = 1
power x p = x * power x (p-1)


isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime n
  | [x | x <- [2..n-1], n`mod`x == 0] == [] = True
  | otherwise = False


slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib(n-2)


quickFib :: Integer -> Integer
quickFib n = fib n (0,1)
    where fib n (n1,n2)
            | n == 0 = n1
            | otherwise = fib (n-1) (n2, n1+n2)
