{-module Main where
import Prelude hiding (&&)-}


-- index :: Eq a => a -> [a] -> Int
-- index x [] = -1
-- index x (y:ys) = if x==y then 0 else let r=index x ys in if r==(-1) then (-1) else 1+r


index :: Eq a => a -> [a] -> Int
index x [] = -1
index x (y:ys) = if x==y then 0 else let r=index x ys in if r==(-1) then (-1) else 1+r

index' :: Eq a => a -> [a] -> Int
index' x ys = index'' x 0 ys
index'' i x [] = -1
index'' i  x (y:ys) = if x==y then i else index'' x (i+1) ys

listIndex :: Eq a => a -> [a] -> Int
listIndex x xs = retrieve[i | (i,y) <- zip[0..] xs, y==x]

retrieve :: [Int] -> Int
retrieve [] = -1
retrieve (x:xs) = x

eval (List es) env = map (ev es)
where ev e = eval e env
