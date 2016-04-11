myLength :: [Int] -> Int -- donada una llista d’enters, calculi la seva llargada.
myLength [] = 0
myLength (_:[]) = 1
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int-- donada una llista d’enters no buida, calculi el seu màxim.
myMaximum [] = error "Una llista buida no té màxim"
myMaximum (x:[]) = x
myMaximum (x:xs)
    | x > m = x
    | otherwise = m
    where
        m = myMaximum xs


average :: [Int] -> Float -- donada una llista d’enters no buida, calculi la seva mitjana.
average (x:[]) = (fromIntegral x)/1.0
average (xs) = (fromIntegral (suma xs)) / (fromIntegral (myLength xs))
    where
        suma (x:[]) = x
        suma (x:xs) = x + suma xs


buildPalindrome :: [Int] -> [Int] -- donada una llista, retorni el palíndrom que comença amb la llista invertida.
buildPalindrome xs = pal xs ++ xs
    where
        pal [] = []
        pal (x:xs) = pal xs ++ [x]


remove :: [Int] -> [Int] -> [Int] -- donada una llista d’enters x i una llista d’enters y, retorna la llista x havent eliminat totes les ocurrències dels elements en y.
remove [] ys = []
remove (x:xs) ys
    | x `notin` ys = [x] ++ remove xs ys
    | otherwise = remove xs ys
    where
        notin _ [] = True
        notin x (y:ys) = y /= x && notin x ys



flatten :: [[Int]] -> [Int] -- aplana una llista de llistes produint una llista d’elements.
flatten [] = []
flatten (l:xs) = l ++ flatten xs


oddsNevens :: [Int] -> ([Int],[Int]) -- donada una llista d’enters, retorni dues llistes, una que conté els parells i una que conté els senars, en el mateix ordre relatiu que a l’original.
oddsNevens xs = addvals xs ([],[])
    where
        addvals [] (a,b) = (a++[],b++[])
        addvals (x:xs) (a,b)
            | odd x = addvals xs (a++[x],b)
            | even x = addvals xs (a,b++[x])





primeDivisors :: Int -> [Int] -- retorni la llista de divisors primers d’un enter estrictament positiu.
primeDivisors n = addivs n 2
    where
        addivs 1 _ = []
        addivs n n2
            |n`mod`n2 == 0 = n2:addivs (n `div` n2) n2
            | otherwise = addivs n (n2+1)
