myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f n [] = n
myFoldl f n (x:xs) = myFoldl f (f n x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ n [] = n
myFoldr f n (x:xs) = f x (myFoldr f n xs)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil fb f x = go x
    where go x
            |fb x = x
            |otherwise = go (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x l -> f x : l) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fs xs = foldr (\x l -> if fs x then x:l else l) [] xs

myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs = foldr (&&) True (map f xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (||) False (map f xs)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map (\(a,b) -> f a b) (zip xs ys)
