module ContLabel where

infixl 1 $>

type M a = (a -> Answer) -> Answer

type Cont a = a -> Answer

result :: a -> M a
result x k = k x

($>) :: M a -> (a -> M b) -> M b
(xm $> f) k = xm (\ x -> f x k)

type Answer = Integer

label :: M Answer
label k = k (label k)

goto :: Answer -> M ()
goto k l = k

test1 = label $> \l -> goto l

gotoparam :: Cont a -> a -> M a
gotoparam k x l = k x

labelparam :: a -> M (a,Cont a)
labelparam x k = k (x,\x -> labelparam x k)

test2 =  labelparam (5,1) $> \((x,r),l) -> if x==0 then result (x,r) else gotoparam l (x-1,r*x)




--testb =  newcc $> \x -> newcc $> \r -> putcc x 5 $> \() -> putcc r 1 $> \() -> (label () $> \((),l) -> getcc x $> \xv -> getcc r $> \rv -> if xv==0 then result() else putcc x (xv-1) $> \_ -> putcc r (rv*xv) $> \_ -> goto l ()) $> \_ -> getcc r


