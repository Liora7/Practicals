module IO where


infixl 1 $>

type Value=Integer

data M a = Done a
         | Input (Value -> M a)
         | Output (Value, M a)

($>) :: M a -> (a -> M b) -> M b
(Done x) $> f = f x
(Input f) $> g = Input (\n -> f n $> g)
(Output (n,xm)) $> g = Output(n,xm$>g)

result :: a -> M a
result x = Done x

out :: Value -> M ()
out n = Output (n,Done ())

inp :: M Value
inp = Input (\n -> Done n)

pipe :: M a -> M a -> M a
pipe xm (Done y) = Done y
pipe xm (Input f) = pipe2 xm f
pipe xm (Output (n,ym)) = Output(n,pipe xm ym)

pipe2 :: M a -> (Value -> M a) -> M a
pipe2 (Done x) f = Done x
pipe2 (Input g) f = Input (\n -> pipe2 (g n) f)
pipe2 (Output (n ,xm)) f = pipe xm (f n)

run :: M a -> IO a 
run (Done x) = return x
run (Input f) = do n <- readLn ; run (f n)
run (Output (n,xm)) = do putStrLn(show n) ; run xm

runST :: M a -> Value -> (a, Value)
runST (Done x) v = (x,v)
runST (Input f) v = runST (f v) v
runST (Output (n,xm)) v = runST xm n