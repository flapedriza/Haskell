insert :: [Int] -> Int -> [Int]  --, donada una llista ordenada i un element, insereixi ordenadament el nou element a la llista.
insert xs x = let junta (a,b) = a ++ x:b in junta (menmaj xs x)
menmaj :: Ord a => [a] -> a -> ([a],[a])
menmaj xs x = ((menors xs),(majors xs))
    where
        menors [] = []
        menors (n:xs)
            |n <=x = n:(menors xs)
            |otherwise = menors xs
        majors [] = []
        majors (n:xs)
            |n > x = n:majors xs
            |otherwise = majors xs



isort :: [Int] -> [Int]  -- implementi l’algorisme d’ordenació per inserció utilitzant la funció anterior.
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs) x


remove :: [Int] -> Int -> [Int]  --, donada una llista i un element x, elimini la primera ocurrència de x de la llista. Podeu assumir  -- l’element sempre és a la llista.
remove [] _ = []
remove (x:xs) n
    | x == n = xs
    | otherwise = x:(remove xs n)


ssort :: [Int] -> [Int]  -- implementi l’algorisme d’ordenació per selecció utilitzant la funció anterior.
ssort [] = []
ssort xs = let x = maximum xs in ssort (remove xs x) ++ [x]


merge :: [Int] -> [Int] -> [Int]  --, donades dues llistes ordenades, les fusioni per obtenir una llista amb tots els seus elements ordenats.
merge xs [] = xs
merge [] ys = ys
merge lx@(x:xs) ly@(y:ys)
    | x <= y = x:merge xs ly
    | otherwise = y:merge lx ys

split :: [Int] -> ([Int],[Int])
split [x] = ([x],[])
split [] = ([],[])
split (x:y:zs) = let (xs, ys) = split zs in (x:xs, y:ys)


msort :: [Int] -> [Int]  -- implementi l’algorisme d’ordenació per fusió utilitzant la funció anterior.
msort [] = []
msort [x] = [x]
msort xs = let (a,b) = split xs in merge (msort a) (msort b)

qsort :: [Int] -> [Int]  -- implementi l’algorisme d’ordenació ràpida.
qsort [] = []
qsort (x:xs) = junta (menmaj xs x)
    where junta (a,b) = qsort a ++ [x] ++ qsort b


genQsort :: Ord a => [a] -> [a]  -- ordeni llistes de qualsevol tip
genQsort [] = []
genQsort (x:xs) = let junta (a,b) = genQsort a ++ [x] ++ genQsort b in junta (menmaj xs x)
