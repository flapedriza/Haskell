data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

--Definicions arbres
t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2

size :: Tree a -> Int --donat un arbre, retorni la seva talla, és a dir, el nombre de nodes que conté.
size Empty = 0
size (Node _ esq dre) = 1 + (size esq) + (size dre)
height :: Tree a -> Int --donat un arbre, retorni la seva alçada, assumint que els arbres buits tenen alçada zero.
height Empty = 0
height (Node _ esq dre) = 1 + (max (height esq) (height dre))

equal :: Eq a => Tree a -> Tree a -> Bool --donat dos arbres, indiqui si són el mateix.
equal Empty Empty = True
equal Empty _ = False
equal _ Empty = False
equal (Node a esq dre) (Node b esq2 dre2) = (a == b) && (equal esq esq2) && (equal dre dre2)

isomorphic :: Eq a => Tree a -> Tree a -> Bool --donat un arbres, indiqui si són el isomorfs, és a dir, si es pot obtenir l’un de l’altre tot girant algun dels seus fills.
isomorphic tree tree2 = areEqual (preOrder tree) (preOrder tree2)
    where
        checkContainsAll [] y = True
        checkContainsAll (x:xs) ys = (elem x ys) && (checkContainsAll xs ys)
        areEqual xs ys = checkContainsAll xs ys && checkContainsAll ys xs

preOrder :: Tree a -> [a] --donat un arbre, retorni el seu recorregut en pre-ordre.
preOrder Empty = []
preOrder (Node a esq dre) = a : preOrder esq ++ preOrder dre

postOrder :: Tree a -> [a] --donat un arbre, retorni el seu recorregut en post-ordre.
postOrder Empty = []
postOrder (Node a esq dre) = postOrder esq ++ postOrder dre ++ [a]

inOrder :: Tree a -> [a] --donat un arbre, retorni el seu recorregut en in-ordre.
inOrder Empty = []
inOrder (Node a esq dre) = inOrder esq ++ [a] ++ inOrder dre

breadthFirst :: Tree a -> [a] --donat un arbre, retorni el seu recorregut per nivells.
breadthFirst tree = bfs [tree]
    where
        bfs [] = []
        bfs xs = (map valnode xs) ++ (bfs (concat (map subarbres xs)))
        valnode (Node a _ _) = a
        subarbres (Node _ Empty Empty) = []
        subarbres (Node _ a Empty) = [a]
        subarbres (Node _ Empty a) = [a]
        subarbres (Node _ a b) = [a,b]

build :: Eq a => [a] -> [a] -> Tree a --donat el recorregut en pre-ordre d’un arbre i el recorregut en in-ordre del mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
build [] _ = Empty
build (x:xs) l2 = (Node x (build esq_pre esq_in) (build dre_pre dre_in))
    where
        (esq_in,_:dre_in) = span (/=x) l2
        (esq_pre, dre_pre) = splitAt (length esq_in) xs

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a --donats dos arbres, retorni la seva superposició utilitzant una funció. Superposar dos arbres amb una funció consisteix en posar els dos arbres l’un damunt de l’altre i combinar els nodes doble resultants amb la funció donada o deixant els nodes simples tal qual.
overlap _ Empty Empty = Empty
overlap _ Empty tree = tree
overlap _ tree Empty = tree
overlap f (Node a esq dre) (Node b esq2 dre2) = Node (f a b) (overlap f esq esq2) (overlap f dre dre2)
