eql :: [Int] -> [Int] -> Bool -- indiqui si dues llistes d’enters són iguals.
eql xs ys
    |length xs /= length ys = False
    |otherwise =  foldr (&&) True (zipWith (==) xs ys)

prod :: [Int] -> Int -- calculi el producte dels elements d’una llista d’enters.
prod xs = foldr (*) 1 xs

prodOfEvens :: [Int] -> Int -- multiplica tots el nombres parells d’una llista d’enters.
prodOfEvens xs = prod (filter even xs)

powersOf2 :: [Int] -- generi la llista de totes les potències de 2.
powersOf2 = map (2^) [0..]

scalarProduct :: [Float] -> [Float] -> Float -- calculi el producte escalar de dues llistes de reals de la mateixa mida.
scalarProduct xs ys = foldr (+) 0 (zipWith (*) xs ys)
