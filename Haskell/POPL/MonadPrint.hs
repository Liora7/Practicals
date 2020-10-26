module MonadPrint where

infixl 1 $>

type M a = (String , a)

result :: a -> M a
result x = ("", x)

($>) :: M a -> (a -> M b) -> M b
($>) xm f =
  let (s,x) = xm in let (t,y) = f x in (s ++ t, y)

mprint :: String -> M()
mprint s = (s,())

test = mprint "hello "
     $> \_ ->
     mprint "fun "
     $> \_ ->
     mprint "world!"
     
