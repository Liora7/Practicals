module ContMaybe where
  datatype MExn a = Ok a | Fail
 
  type M a = (a -> MExn Answer) -> MExn Answer
  ($>) :: M a -> (a -> M b) -> M b
  (xm $> f) k = xm (\x -> f x k)
  result :: a -> M a
  result a k = k a

  orelse :: M a -> M a -> M a
  orelse xm ym k =
    case xm k of Just x -> Just x
                 Nothing -> ym k

  failure :: M a
  failure k = Nothing
  
  type Answer = Integer