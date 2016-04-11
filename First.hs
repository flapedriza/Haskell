double :: Int -> Int
double x = double2 x 0
double2 :: Int -> Int -> Int
double2 x y = x*2 + y*2
firstmultiplesof :: Int -> Int -> [Int]
firstmultiplesof r n = [x | x <- [1..r*n], mod x n == 0]
fib :: Int -> Int
fib x = if x == 1 || x == 0 then x else ((fib (x-1)) + (fib (x-2)))
multiplylists :: [Int] -> [Int] -> [Int]
multiplylists l1 l2 gh= [x*y | x <- l1, y <- l2]
nevennums :: Int -> [Int]
nevennums n = [x | x <- [1..n], even x]
triangles :: [(Integer,Integer,Integer)]
triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles :: [(Integer,Integer,Integer)]
rightTriangles = [(a,b,c) | c <- [1..10], b<-[1..c], a<-[1..c], a^2+b^2==c^2]
factorial :: Integer -> Integer
factorial x = if x == 1 then 1 else x*factorial (x-1)
addvectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addvectors (x1,x2) (y1,y2) = (x1 + y1, x2 + y2)
second :: (a, b, c) -> b
second (_,x,_) = x
sumpairs xs = [a+b | (a,b) <- xs]
maxlist  :: (Ord a) => [a] -> a
maxlist [] = error "Empty lists do not have max"
maxlist (x:[]) = x
maxlist (x:xs)
| x > max' = x
    |otherwise = max'
    where max' = maxlist xs
compare' :: (Num a, Show a, Ord a) => a -> a -> String
compare' x y
    | compare x y == EQ = show x ++ " is equal to " ++ show y
    | compare x y == GT = show x ++ " is greater than " ++ show y
    | compare x y == LT = show x ++ " is lower than " ++ show y
