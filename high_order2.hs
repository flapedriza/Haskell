flatten :: [[Int]] -> [Int] -- aplana una llista de llistes d’enters en una llista d’enters.
flatten xs = foldr (++) [] xs
myLength :: String -> Int -- retorna la llargada d’una cadena de caràcters.
myLength xs = foldr (+) 0 (map (\c -> 1) xs)
myReverse :: [Int] -> [Int] -- inverteix els elements d’una llista d’enters.
myReverse xs = foldl (flip (:)) [] xs
countIn :: [[Int]] -> Int -> [Int] --, donada una llista de llistes d’elements ℓ i un element x ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
countIn xs x = map (length . filter (==x)) xs
firstWord :: String -> String --, donat un string amb blancs i caràcacters alfabètics), en retorna la primera paraula.
firstWord xs = takeWhile (/=' ') (dropWhile (==' ') xs)
