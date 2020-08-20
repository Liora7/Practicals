{-module Main where
import Prelude hiding (&&)-}
import Data.List (intercalate)

double :: Int -> Int
double x = 2 * x
-- takes a number x and multiplies it by 2

factorial :: Int -> Int
factorial n = product[1..n]
-- produces a list of all integers up to and including n, and multiplies them all together

choose :: Int -> Int -> Int
choose n r = div (factorial n) (factorial r * factorial(n-r))
-- divides the (previously defined) factorial of n by the product of the factorial of r and the factorial of (n-r), according to the formula for n choose r

check :: Int -> Bool
check n = sum(map (choose n) [0..n])== 2^n
-- uses map to calculate choose n r for all r from 0 to n, then sums all of the results and checks to see if the sum is equal to 2^n

not1 :: Bool -> Bool
not1(True) = False
not1(False) = True
-- defines not1 in terms of its two cases, when input is True and when input is False

not2 :: Bool -> Bool
not2 x
  |x == True = False
  |x == False = True
-- uses guards to test x for equality with first True, and if the equality returns False, tests if x is equal to False

not3 :: Bool -> Bool
not3 x =
  if x == False
    then True
    else False
-- uses an if statement to test if x is False, if it is, not3 returns False, and if it isn't, it automatically returns False

not4 :: Bool -> Bool
not4 x = head[y | y<-[True, False], x/= y]
-- first takes y = True, sees if input x is equal to y (True). If it isn't, not4 returns y, which is True. If it is, it takes y = False and checks if x = y, (which it now shouldn't be) and then returns y, which is now False. This is a singleton list, so head "unpacks" it into a Bool.

square :: Int -> Int
square x = x * x
-- takes x and multiplies it by itself

remain :: Int -> Int
remain x = mod x 3
-- calculates x modulus 3, which is the remainder of x divided by 3

psquare :: Int -> Int
psquare x = last[square n| n <- [1..x], square n <= x]
-- makes a list of the squares of all integers up to x which are less than or equal to x, and then takes the last item of the list (the largest square in the last)

{- instance Ord a => Ord [a] where
  (x:xs) < (y:ys)
      | x < y = True
      | x == y && xs == [] = True
      | x == y && ys == [] = False
      | x == y = xs< ys
      | x > y = False


class Eq a => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  x < y = not (x >= y)
  x > y = not (x <= y)
  x >= y = x == y || x>y -}

song :: Int -> String
song 0 = ""
song n = song(n-1) ++ "\n" ++ verse n
verse n = line1 n ++ "\n" ++ line ++ "\n" ++ line3 n ++ "\n" ++ line ++ "\n"
line1 n = if n == 1 then ns!!n ++ " man went to mow" else ns!!n ++ " men went to mow"
ns = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"]
line = "went to mow a meadow"
line3 0 = ""
line3 n = if n == 1 then "one man and his dog" else ns!!n ++ " men, " ++ line3 (n-1)

subst f g x = (f x) (g x)
fix f = f (fix f)
twice f = f.f
--selfie f = f f

False &&& y = False
x &&& False = False
True &&& True = True

take1 :: Int -> [a] -> [a]
take1 0 xs = []
take1 n [] = []
take1 n (x:xs) = [x] ++ take1 (n-1) xs


drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 n [] = []
drop1 n (x:xs) = drop1 (n-1) xs


evens :: [a] -> [a]
evens xs = [xs!!n | n <- [0,2..y]]
      where y = length(xs) - (mod (length(xs)-1) 2)

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = evens xs


alternates :: [a] -> ([a], [a])
alternates xs = split([],[]) xs
split p (x:y:xs) = split((fst p)++[x], (snd p)++[y]) xs
split p [] = p
split (xs,ys) [x] = (xs++[x],ys)

curry1 :: ((a,b) -> c) -> (a -> b -> c)
curry1 f = \x y -> f(x,y)

uncurry1 :: (a -> b -> c) -> ((a,b) -> c)
uncurry1 f = \(x,y) -> f x y


f' f (a,b) = f a b
f'' f a b = f (a,b)
curry2 f = f'' f
uncurry2 f = f' f


zip1 :: [a] -> [b] -> [(a, b)]
zip1 [] xs = []
zip1 xs [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys


zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith1 f (x:xs) (y:ys) = (f x y) : zipWith1 f xs ys


zip2 :: [a] -> [b] -> [(a, b)]
zip2 xs ys = zipWith (\x y -> (x, y)) xs ys


splits :: [a] -> [(a,[a])]
splits xs = map splitshelp (zip [0..((length xs)-1)] (repeat xs))

splitshelp (n, xs) = (xs!!n, take n xs ++ drop (n+1) xs)

{-
permutations xs = foldr g' [[]] xs

g' (x:xs) a = [zs | ys <- a, zs <- include x ys]


include x ys = foldr f x [x] (makeSubLists ys)

f x (z:zs) a = (x:z:zs) : [map (:z) a]

makeSubLists xs = [drop n xs | n <- [0..l]]
          where l = length xs - 1
-}

unfold :: (a->Bool) -> (a->b) -> (a->a) -> a -> [b]
unfold null head tail x
    | null x  = []
    | otherwise = head x:(unfold null head tail (tail x))


unfold1 :: (a->Bool) -> (a->b) -> (a->a) -> a -> [b]
unfold1 null head tail = map head . takeWhile (not.null) . iterate tail



--6.8

data Tree a = Fork (Tree a) a (Tree a) | Empty

insert :: Ord a => a -> Tree [a] -> Tree [a]
insert x Empty = Fork Empty [x] Empty
insert x (Fork ltree ys rtree)
    | x < (head ys) = Fork (insert x ltree) ys rtree
    | x == (head ys) = Fork ltree (ys ++ [x]) rtree
    | x > (head ys) = Fork ltree ys (insert x rtree)

-- Recursively places x so that x ends up either added onto an existing list with the same value or becomes a new Fork with empty subtrees.


flatten :: Tree [a] -> [a]
flatten Empty = []
flatten (Fork ltree xs rtree) = flatten ltree ++ xs ++ flatten rtree

--Recursively adds on the lists from the Forks until all Forks have been searched.


bsort :: Ord a => [a] -> [a]
bsort = flatten . foldr insert Empty

--Takes values from a list, inserts each of them into an initially Empty tree: inserting causes them to be sorted by size. Then flatten lists the values off by their order in the tree.



--7.1

cp :: [[a]] -> [[a]]
cp xss = foldr h [[]] xss

h :: [a] -> [[a]] -> [[a]]
h as xss = foldr (\a bs -> (map (a:) xss)++bs) [] as

--The helper function h takes a list and a list of lists, and then cons's each element of the list onto the list of lists, and then adds the rest of the first list using a foldr. cp then consists of a foldr with h as its function and a list containing the empty list as its empty value.


--7.2

cols :: [[a]] -> [[a]]
cols [xs] = [ [x] | x <- xs ]
cols (xs:xss) = zipWith (:) xs (cols xss)

cols1' :: [[a]] -> [[a]]
cols1' ([]:_) = []
cols1' xss = (map head xss) : cols1'(map tail xss)

--cols1' takes the first element of each list of xss (column of a matrix) and cons's it onto cols1' of the rest of xss, creating rows out of columns (the transpose of the matrix)


cols2' :: [[a]] -> [[a]]
cols2' = foldr (zipWith (:)) (repeat [])

--cols2' uses a foldr to to zip the columns together and cons them into rows.


--8.1

rjustify :: Int -> String -> String
rjustify n xs = concat(emps (n-length xs)) ++ xs

emps n = take n(repeat " ")


--If the string is bigger than the target length, it just prints the string. The goal however is for it to split the string into substrings of length n, putting each substring into a new line of length n.

rjustify2 :: Int -> String -> String
rjustify2 n "" = ""
rjustify2 n xs = rjustify n (take n xs) ++ "\n" ++ rjustify2 n (drop n xs)


--rjustify2 does this by recursively inserting linebreaks after n chars and rjustifying normally any lines that do not fill up the n chars.


ljustify :: Int -> String -> String
ljustify n xs = xs ++ concat(emps (n-length xs))

ljustify2 :: Int -> String -> String
ljustify2 n "" = ""
ljustify2 n xs = ljustify n (take n xs) ++ "\n" ++ ljustify2 n (drop n xs)



--8.2

type Matrix a = [[a]]

scale :: Num a => a -> Matrix a -> Matrix a
scale x matx = [scal x ys|ys <- matx]
scal x ys = [x*y | y<-ys]

--scale uses a helper function scal which takes a scalar and multiplies all elements of a fector by it. scale applies scal to each column vector of the matrix.


dot :: Num a => [a] -> [a] -> a
dot xs ys = sum(zipWith (*) xs ys)

--dot multiplies the ith element of a vector xs with the ith element of a vector ys and then sums all the products up to get the dot product.


add :: Num a => Matrix a -> Matrix a -> Matrix a
add mata matb = zipWith (zipWith (+)) mata matb

--add uses two zipWiths to first represent the sum of the two vectors as sums of column vectors, and then to add the elements in the column vectors.


mul :: Num a => Matrix a -> Matrix a -> Matrix a
mul mata matb = [[dot xs ys | ys <- tmapb] | xs <- mata]
        where tmapb = cols2' matb

--mul transposes the second matrix and then simply takes the dot product of each row of mata and each column of matb to produce a new matrix based on the definition of matrix multiplication.


table :: Show a => Matrix a -> String
table matx = unlines . map unwords . cols2' $ (map showCol (cols2' matx))
  where showCol :: Show a => [a] -> [String]
        showCol col = map (rjustify maxLen . show) col
           where maxLen = maximum (map (length . show) col)

rrevss :: [a] -> [a]
rrevss = foldr f []
    where f a b = b ++ [a]

data RTree a = Node a [RTree a]
foldRTree :: (a-> [b] -> b) -> RTree a -> b
foldRTree f (Node x ys) = f x (map (foldRTree f) ys)
