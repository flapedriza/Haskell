myMap :: (a -> b) -> [a] -> [b] -- emuli el map usant llistes per comprensió.
myMap f xs = [f a |a<-xs]
myFilter :: (a -> Bool) -> [a] -> [a] -- emuli el filter usant llistes per comprensió.
myFilter f xs = [a |a<-xs, f$a]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- que emuli el zipWith usant llistes per comprensió i zip.
myZipWith f xs ys = [f a b |(a,b)<-(zip xs ys)]
thingify :: [Int] -> [Int] -> [(Int, Int)] --, donades dues llistes d’enters, genera la llista que aparella els elements si l’element de la segona llista divideix al de la primera.
thingify xs ys = [(a,b) |a<-xs, b<-ys, mod a b == 0]
factors :: Int -> [Int] --, donat un natural no nul, genera la llista ordenada amb els seus factors (no necessàriament primers).
factors x = [a |a<-[1..x], x`mod`a == 0]
