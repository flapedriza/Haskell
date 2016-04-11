data Gtree a = Const a | Var String | Func [Gtree a]

allconst :: Gtree a -> [a]
allconst (Const a) = [a]
allconst (Var _) = []
allconst (Func gtree) = foldr (\a b -> a ++ allconst b) [] gtree
