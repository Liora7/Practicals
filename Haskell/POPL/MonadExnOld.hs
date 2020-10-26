module MonadExn where

data M a = Ok a | Fail deriving Show

result :: a -> M a
result x = Ok x

($>) :: M a -> (a -> M b) -> M b
($>) (Ok x) f = f x
($>) Fail f = Fail

orelse :: M a -> M a -> M a
orelse (Ok x) ym = Ok x
orelse Fail ym = ym

dec :: Integer -> M Integer
dec n = if n > 1 then result (n-1) else Fail

fact :: Integer -> M Integer
fact n = orelse (dec n $> \n' -> fact n'
                 $> \r -> result (n*r))
                (result 1)




