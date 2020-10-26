module MaybeCont where
  type M a = (Maybe a -> Answer) -> Answer
  ($>) :: M a -> (a -> M b) -> M b
  (xs $> f) k = xs (\x -> case x of Just x -> f x k
                                    Nothing -> k Nothing)
  result :: a -> M a
  result a k = k (Just a)

  orelse :: M a -> M a -> M a
  orelse xm ym k =
    xm (\x -> case x of Just x -> k (Just x)
                        Nothing -> ym k)

  failure :: M a
  failure k = k Nothing
  
  type Answer = Maybe Integer