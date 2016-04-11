data Queue a = Queue [a] [a] deriving Show

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue l l2) = Queue l (x:l2)

pop :: Queue a -> Queue a
pop (Queue [] l2) = Queue (reverse $ init l2) []
pop (Queue l l2) = Queue (tail l) l2


top :: Queue a -> a
top (Queue [] l) = last l
top (Queue l _) = head l

empty :: Queue a -> Bool
empty (Queue l1 l2) =
    let isEmpty [] = True
        isEmpty _ = False
    in isEmpty l1 && isEmpty l2

instance Eq a => Eq (Queue a) where
    (Queue l l2) == (Queue l3 l4) = (l++(reverse l2)) == (l3++(reverse l4))
