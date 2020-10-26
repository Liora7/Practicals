module MonadND where
import Data.List

infixl 1 $>

type M a = [a]

result :: a -> M a
result x = [x]

($>) :: M a -> (a -> M b) -> M b
($>) x f = concatMap f x 
  
dunno :: M Int
dunno = [ 0, 1]



