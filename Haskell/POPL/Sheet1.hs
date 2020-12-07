{-module Main where
import Prelude hiding (&&)-}

revk :: [Int] -> ([Int] -> Answer) -> Answer
revk [] k = k []
revk (x:xs) k = revk xs (\r -> k(r++[x]))

revi :: [Int] -> [Int]
revi xs = revi' xs []

revi' :: [Int] -> [Int] -> [Int]
revi' [] ys = ys
revi' (x:xs) ys = revi' xs (x:ys)

data Cont =
  Show | Sum Int Cont | Fib Int Cont

apply :: Cont -> Int -> Int
apply Show x = x
apply (Sum n k) x = apply k (n+x)
apply (Fib n k) x = apply Sum((fibk n id) k) x

fibk n k =
if n <= 1 then apply k n else fibk (n - 1) (Fib (n-2) k)



-- index :: Eq a => a -> [a] -> Int
-- index x [] = -1
-- index x (y:ys) = if x==y then 0 else let r=index x ys in if r==(-1) then (-1) else 1+r
import Memory


index :: Eq a => a -> [a] -> Int
index x [] = -1
index x (y:ys) = if x==y then 0 else let r=Main.index x ys in if r==(-1) then (-1) else 1+r

--index' :: Eq a => a -> [a] -> Int
index' x ys = index'' x 0 ys
index'' i x [] = -1
index'' i  x (y:ys) = if x==y then i else index'' x (i+1) ys

listIndex :: Eq a => a -> [a] -> Int
listIndex x xs = retrieve[i | (i,y) <- zip[0..] xs, y==x]

retrieve :: [Int] -> Int
retrieve [] = -1
retrieve (x:xs) = x

--eval (List es) env = map (ev es)
--where ev e = eval e env


data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Addr Location
  | Function ([Value] -> M Value)

infixl 1 $>

type Mem = Memory Value

type M a = Mem -> (String, a , Mem)

result :: a -> M a
result x mem = ("", x , mem)

($>) :: M a -> (a -> M b) -> M b
($>) xm f mem = let (s, x , mem') = xm mem in let (t, y, mem'') = f x mem' in (s++t, y, mem'')

get :: Location -> M Value
get a mem = ("", contents mem a, mem)

put :: Location -> Value -> M()
put a v mem = ("", (), update mem a v)

new :: M Location
new mem = let (o, m) = fresh mem in ("", o, m)

output :: String -> M()
output s mem = (s, (), mem)

eval (Apply f es) env =
  revevalargs es env $> (\ args ->
    eval f env $> (\ fv ->
      apply fv args))
  where revevalargs es env = reverse (evalargs (reverse es) env)
