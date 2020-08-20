-- 11.1
-- data Bool = False | True
foldB :: a -> a -> Bool -> a
foldB false true False = false
foldB false true True = true


data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

foldDay :: a -> a -> a -> a -> a -> a -> a -> Day -> a
foldDay sun mon tue wed thu fri sat Sunday = sun
foldDay sun mon tue wed thu fri sat Monday = mon
foldDay sun mon tue wed thu fri sat Tuesday = tue
foldDay sun mon tue wed thu fri sat Wednesday = wed
foldDay sun mon tue wed thu fri sat Thursday = thu
foldDay sun mon tue wed thu fri sat Friday = fri
foldDay sun mon tue wed thu fri sat Saturday = sat


--11.2
{-
x <= y = if (x < y || x == y) then True else False
-}


--11.3
data Set a = Empty | Singleton a | Union (Set a) (Set a)

foldSet :: a -> (b -> a) -> (a -> a -> a) -> Set b -> a
foldSet n sing comp Empty = n
foldSet n sing comp (Singleton a) = sing a
foldSet n sing comp (Union xs ys) = comp (foldSet n sing comp xs) (foldSet n sing comp ys)


isIn :: Eq a => a -> Set a -> Bool
isIn x ys = foldSet (False) (==x) (||) ys


subset :: Eq a => Set a -> Set a -> Bool
subset xs ys = foldSet (True) (test ys) (&&) xs
    where test ys x = isIn x ys

instance Eq a => Eq (Set a) where
    xs == ys = (xs `subset` ys) && (ys `subset` xs)



--11.4

data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving Show
data Direction = L | R deriving (Show, Eq)
type Path = [ Direction ]

foldBT :: (a -> b) -> (b -> b -> b) -> BTree a -> b
foldBT leaf fork = f
    where f (Leaf x) = leaf x
          f (Fork l r) = fork (f l) (f r)

find :: Eq a => a -> BTree a -> Maybe Path
find x t = if (path == []) then Nothing else (Just path)
    where path = test [] x t

test :: Eq a => Path -> a -> BTree a -> Path
test path x (Leaf a) = if (x==a) then path else []
test path x (Fork l r)
    | foldBT (==x) (||) l = test (path ++ [L]) x l
    | foldBT (==x) (||) r = test (path ++ [R]) x r
    | otherwise = []


--12.1
data Queue a = Emp | Snoc (Queue a) a   deriving (Eq, Show)

empty :: Queue a
empty = Emp
-- This runs in constant time, O(0), as empty simply always creates an empty list and takes no arguments

isEmpty :: Eq a => Queue a -> Bool
isEmpty q = q==Emp
-- isEmpty also runs in constant time, as it checks whether q starts with thr constructor Emp, and if it doesnt, isEmpty doesn't have to check the rest of q.

add :: a -> Queue a -> Queue a
add x q = Snoc q x
-- add should run in constant time as well, as it doesn't actually go through the queue, but instead simply adds another constructor in front of it.

get :: Queue a -> (a, Queue a)
get (Snoc Emp a) = (a, Emp)
get (Snoc q a) = (x, (Snoc y a))
    where (x,y) = get q
-- get runs in roughly linear time as it calls on get once for every element in the queue. If queues were represented in the reverse order, get would run in constant time as it would simply have to pop off the first element of the queue (undo a Snoc).


data Queue' a = Queue' [a] [a]    deriving (Eq, Show)

empty' :: Queue' a
empty' = Queue' [] []
-- still constant

isEmpty' :: Eq a => Queue' a -> Bool
isEmpty' q = q==(Queue' [] [])
-- also constant

add' :: a -> Queue' a -> Queue' a
add' x (Queue' xs []) = Queue' (x:(take n xs)) (reverse(drop n xs))
    where n = quot (length xs) 2
add' x (Queue' [] ys) = Queue' (x:(reverse(take n ys))) (drop n ys)
    where n = quot (length ys) 2
add' x (Queue' xs ys) = Queue' (x:xs) ys
-- can be up to linear in xs or ys but aims to balance the front and back lists to avoid this on the next call of add'

get' :: Queue' a -> (a, Queue' a)
get' (Queue' xs []) = (last xs, Queue' (init xs) [])
get' (Queue' xs (y:ys)) = (y, Queue' xs ys)
-- is linear in xs if ys is empty, otherwise constant


--12.2
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-  ghci> fib 10
    55
    (0.01 secs, 130,864 bytes)

    ghci> fib 20
    6765
    (0.02 secs, 7,565,208 bytes)

    ghci> fib 30
    832040
    (1.72 secs, 921,594,056 bytes) -}

two 0 = (0,1)
two n = (snd(n1), fst(n1) + snd(n1))
  where n1 = two (n-1)

fib' 0 = 0
fib' n = sum(two (n-1))
{-  ghci> fib' 10
    55
    (0.01 secs, 75,760 bytes)

    ghci> fib' 20
    6765
    (0.01 secs, 83,216 bytes)

    ghci> fib' 30
  832040
  (0.01 secs, 87,840 bytes) -}
-- calculating fib' n is roughly linear in n.


roughly :: Integer -> String
roughly n = [head xs] ++ "e" ++ show (length xs - 1)
    where xs = show n

{-  ghci> roughly(fib' 10000)
    "3e2089"
    (0.02 secs, 10,378,456 bytes) -}


{- F =  (0 1)
        (1 1)

Multiplying F by itself means "moving" the bottom left number to the top left, moving the bottom right number to the top right, adding the two numbers in the left column and putting the sum in the bottom left, and doing the same for the right column. By definition of Fib numbers, the sum of the two previous numbers is the next Fib number. As Fib 0 = 0 and Fib 1 = 1, we can see that
  F = (Fib 0  Fib 1)
      (Fib 1  Fib 2)
So by the process described above,
F^2 = (Fib 1              Fib 2        )
      ((Fib 0 + Fib 1)  (Fib 1 + Fib 2))
    = (Fib 1  Fib 2)
      (Fib 2  Fib 3)
And so on. Then
F^n = (Fib (n-1)    Fib n  )
      (Fib n      Fib (n+1))
-}


power (*) y x n -- x^n*y
    | n == 0 = y
    | even n = power (*) y (x*x) (n `div` 2)
    | odd n = power (*) (x*y) x (n-1)


type Matrix a = [[a]]

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum(zipWith (*) xs ys)

cols2' :: [[a]] -> [[a]]
cols2' = foldr (zipWith (:)) (repeat [])

mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul mata matb = [[dot xs ys | ys <- tmapb] | xs <- mata]
        where tmapb = cols2' matb

f = [[0,1],[1,1]]
ret = [[0], [1]]

fibM n = (head.head) (power mul ret f n)
--2logn matrix mults

{-
  ghci> roughly (fibM 1000000)
  "1e208987"
  (0.03 secs, 14,274,776 bytes)
-}
