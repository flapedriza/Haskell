--Generar la seqüència dels uns [1,1,1,1,1,1,1,1,…].
--Generar la seqüència dels naturals [0,1,2,3,4,5,6,7…].
--Generar la seqüència dels enters [0,1,−1,2,−2,3,−3,4…].
--Generar la seqüència dels nombres triangulars: 0,1,3,6,10,15,21,28,…].
--Generar la seqüència dels nombres factorials: [1,1,2,6,24,120,720,5040,…].
--Generar la seqüència dels nombres de Fibonacci: [0,1,1,2,3,5,8,13,…].
--Generar la seqüència dels nombres primers: [2,3,5,7,11,13,17,19,…].
--Generar la seqüència ordenada dels nombres de Hamming: [1,2,3,4,5,6,8,9,…]. Els nombres de Hamming són aquells que només tenen 2, 3 i 5 com a divisors primers.
--Generar la seqüència mira i digues: [1,11,21,1211,111221,312211,13112221,1113213211,…].
--Generar la seqüència de les files del triangle de Tartaglia (també anomenat triangle de Pascal): [[1],[1,1],[1,2,1],[1,3,3,1],…].

ones :: [Integer]
ones = 1: [x | x <- ones]

nats :: [Integer]
nats = iterate succ 0

ints :: [Integer]
ints = iterate (\x -> if x <= 0 then succ (abs x) else x*(-1)) 0

triangulars :: [Integer]
triangulars = map (\x -> div (x*(x-1)) 2) (iterate succ 1)

factorials :: [Integer]
factorials = 1: map (\x -> foldr1 (*) (take (fromIntegral x) (iterate succ 1))) (iterate succ 1)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = let garbell (x:xs) = x : garbell (filter (\y -> mod y x /= 0) xs)
        in garbell (iterate succ 2)

hammings :: [Integer]
hammings = 1 :  (map (2*) hammings) `merge` (map (3*) hammings) `merge` (map (5*) hammings)
    where
        merge xxs@(x:xs) yys@(y:ys)
            |x < y = x : merge xs yys
            |x == y = x : merge xs ys
            |otherwise = y : merge xxs ys

            lookNsay :: [Integer]
            -- https://en.wikipedia.org/wiki/Look-and-say_sequence
            lookNsay = iterate (toNum.blocks.toList) 1
               where
                  toList :: Integer -> [Integer]
                  toList 0 = []
                  toList n = (mod n 10):(toList (div n 10))
                  blocks :: [Integer] -> [(Integer,Integer)]
                  blocks [] = []
                  blocks l = (\(n1,n2) -> (toInteger (length n1), head n1):(blocks n2)) (span (==(head l)) l)
                  toNum :: [(Integer,Integer)] -> Integer
                  toNum [] = 0
                  toNum ((rep,dig):s) = 100*(toNum s) + 10*rep + dig

--tartaglia :: [[Integer]]
