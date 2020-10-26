module MonadMem where
import Data.List

infixl 1 $>

type M a = [Int] -> (a, [Int])
type Location = Int

result :: a -> M a
result x = \mem -> (x,mem)

($>) :: M a -> (a -> M b) -> M b
($>) xm f =
  \mem -> let (y,mem') = xm mem in f y mem'

put :: Location -> Int -> M ()
put a v = \mem -> ((),(take a mem) ++ v : (drop (a+1) mem))

get :: Location -> M Int
get a = \mem -> (mem !! a , mem)

new :: M(Location)
new = \mem -> (length mem,mem++[0])

test = new $> \a ->
     put a 40 $> \_ ->
     get a $> \v ->
     put a (v+1) $> \_ ->
     get a $> \v -> result (v+1)
     

