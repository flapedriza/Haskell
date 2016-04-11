
import Control.Monad

sumaQUadrats :: (Num a) => [a] -> a
sumaQUadrats xs = foldr1 (\x y -> x + y^2) xs

subsWhile n a b
    |a > b = subsWhile (n+1) (a-b) b
    |a == b = n+1
    |otherwise = n

func (x:xs) = do
    var <- return (3)
    return ((x*var) : func xs)

interpretCommand m [10] (Seq [(Input "b"),(Loop (NOT (Eq (Var "a") (Var "b"))) (Assign "a" (Plus (Var "a") (Const 1))))])

a = 12
a = 3
output = [3]
b = 213
