fib :: Integer -> Integer
fib x = if x <= 1 then x else fib(x-1) + fib(x-2)

fibk :: Integer -> (Integer -> a) -> a 
fibk x k = if x <= 1 then k x else fibk (x-1) (\r -> fibk (x-2) (\s -> k (r + s)))

fact :: Integer -> Integer
fact x = if x == 0 then 1 else x * fact (x-1)

factk :: Integer -> (Integer -> a) -> a
factk x k = if x == 0 then k 1 else factk (x-1) (\r -> k (r * x))