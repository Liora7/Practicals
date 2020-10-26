module MonadExn where

infixl 1 $>

data M a = Ok a | Fail deriving Show

result :: a -> M a
result x = Ok x

($>) :: M a -> (a -> M b) -> M b
($>) (Ok x) f = f x
($>) (Fail) f = Fail
  




