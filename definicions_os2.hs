countIf :: (Int -> Bool) -> [Int] -> Int -- donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements de la llista que satisfan el predicat.
--Nota: Aquesta funció d’ordre superior existeix en llenguatges de tractament de fulls de càlcul com ara EXCEL.
countIf f xs = length $ filter f xs

pam :: [Int] -> [Int -> Int] -> [[Int]] -- donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes resultant d’aplicar cada una de les funcions de la segona llista als elements de la primera llista.
pam lx lf = [map f lx | f <- lf]

pam2 :: [Int] -> [Int -> Int] -> [[Int]] -- donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
pam2 lx lf = [[f x | f <- lf]| x <- lx]

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int -- fa el plegat dels elements que satisfan la propietat donada.
filterFoldl f f2 n xs = foldl f2 n (filter f xs)

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] -- donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.
insert rel xs n = (filter (not.rel n) xs) ++ [n] ++ (filter (rel n) xs)

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] -- ordeni la llista per inserció segons la relació donada.
insertionSort f = foldr (flip (insert f))  []
