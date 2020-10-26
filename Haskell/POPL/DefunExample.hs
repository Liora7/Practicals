module DefunExample where


   data FUNCTION = TWICE | SQUARE

   apply TWICE x = x*2
   apply SQUARE x = x*x

   mymap f [] = []
   mymap f (x : xs) = (apply f x) : (mymap f xs)