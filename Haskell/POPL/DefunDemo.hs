module DefunDemo where

  data FUNCTION = TWICE | SQUARE | ADDN Integer

  apply TWICE x = x * 2
  apply SQUARE x = x * x
  apply (ADDN n) x = x + n 

  mymap f [] = []
  mymap f (x : xs) = (apply f x) : (mymap f xs) 

  interval m n = if m>=n then [] else m : interval (m+1) n
  
  testDouble = mymap TWICE (interval 1 10)
  testSquare = mymap SQUARE (interval 1 10)
  testAddN n = mymap (ADDN n) {-- (\x -> x+n)--} (interval 1 10)
  
