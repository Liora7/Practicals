module Cont where

infixl 1 $>

type M a = (a -> Answer) -> Answer

result :: a -> M a
result x k = k x

($>) :: M a -> (a -> M b) -> M b
(xm $> f) k = xm (\ x -> f x k)

type Answer = Integer