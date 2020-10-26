module Productcps where

  infixl 1 $>

  type M a = (a -> Answer) -> Answer -> Answer

  result :: a -> M a 
  result x k l = k x 

  ($>) :: M a -> (a -> M b) -> M b
  (xm $> f) k l = xm (\x -> f x k l) l

  failure :: M a
  failure k l = l

  orelse :: M a -> M a -> M a
  orelse xm ym k l = xm k (ym k l)

  type Answer = Integer

  productjump [] k l = k 1
  productjump (x : xs) k l = if x==0 then l else
                           productjump xs (\n -> k (x * n)) l

  productjumpmonad [] = result 1
  productjumpmonad (x : xs) = if x==0 then failure else
                           productjumpmonad xs $> \n -> result (x * n)



  productcps [] k = k 1
  productcps (x : xs) k = productcps xs (\n -> k (x * n))

  productmonad [] = result 1
  productmonad (x : xs) = productmonad xs $> \n -> result (x * n)


