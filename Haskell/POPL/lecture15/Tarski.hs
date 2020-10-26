module Tarski where

  -- The vertical natural numbers.
  -- NB Haskell implicitly allows the looping program as an inhabitant of every type
  -- And allows infinite instances of datatypes.
  data Vertical = Succ Vertical

  -- top is the infinite sequence of Succ's.
  top :: Vertical
  top = Succ top

  -- nat gives a sequence of n succ's followed by looping forever
  -- In particular, nat 0 is bottom.
  nat :: Integer -> Vertical
  nat 0 = nat 0
  nat n = Succ (nat (n-1))

  -- Approx n f gives f^n(bottom)
  approx :: Vertical -> (a -> a) -> a
  approx (Succ n) f = f (approx n f)

  -- A chain in a is just a function Vertical -> a. 
  -- The least upper bound is the value of the chain at top
  lub :: (Vertical -> a) -> a
  lub xs = xs top

  -- Tarski's fixed point theorem defines the fixed point
  -- as a least upper bound
  tarskifix :: (a -> a) -> a
  tarskifix f = lub (\n -> approx n f)

  -- Example: g(factorial) = factorial
  -- Try (tarskifix g) 5
  -- Try approx (nat 6) g 5
  -- Try approx (nat 2) g 5
  g(f) = \n -> if n==0 then 1 else n * f(n-1)