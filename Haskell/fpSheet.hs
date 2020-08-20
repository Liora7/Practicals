data Nat = Zero | Succ Nat deriving (Eq, Show)

int :: Nat -> Int
int Zero = 0
int (Succ x) = int x + 1

nat :: Int -> Nat
nat 0 = Zero
nat (x) = Succ (nat (x-1))


add, mul, pow, tet :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = Succ (add x y)

mul x (Succ Zero) = x
mul x (Succ y) = add x (mul x y)

pow x Zero = Succ Zero
pow x (Succ y) = mul x (pow x y)

tet x (Succ Zero) = x
tet x (Succ y) = pow x (tet x y)


foldNat :: (a -> a) -> a -> Nat -> a
foldNat cons nil Zero = nil
foldNat cons nil (Succ x) = cons (foldNat cons nil x)

--foldNat is characterised by the constructors of Nats, Zero and Succ. foldNat (Succ) Zero is the identity.


unfoldNat :: (a -> Bool) -> (Nat -> Nat) -> (a -> a) -> a -> Nat
unfoldNat null head tail x
    | null x = Zero
    | otherwise = head (unfoldNat null head tail (tail x))

--unfoldNat is characterised by Zero. unfoldNat (==Zero) id id is the identity.


int' :: Nat -> Int
int' = foldNat (+1) 0

nat' :: Int -> Nat
nat' = unfoldNat (==0) Succ dec


dec :: Int -> Int
dec x = x - 1

add' :: Nat -> Nat -> Nat
add' x y = foldNat Succ x y

mul' :: Nat -> Nat -> Nat
mul' x y = foldNat (add' x) Zero y

pow' :: Nat -> Nat -> Nat
pow' x y = foldNat (mul' x) (Succ Zero) y

tet' :: Nat -> Nat -> Nat
tet' x y = foldNat (pow' x) (Succ Zero) y


data Liste a = Snoc (Liste a) a | Lin deriving (Eq, Show)

cat :: Liste a -> Liste a -> Liste a
cat Lin xs = xs
cat (Snoc xs a) b = Snoc (cat xs b) a


folde :: (b -> a -> b) -> b -> Liste a -> b
folde cons nil Lin = nil
folde cons nil (Snoc a b) = cons (folde cons nil a) b


cat' :: Liste a -> Liste a -> Liste a
cat' a b = folde Snoc a b


list :: Liste a -> [a]
list = reverse.folde (flip (:)) []

liste :: [a] -> Liste a
liste xs = foldr (flip Snoc) Lin (reverse xs)

--liste does not return anything when called on an infinite list
--There are no infinite objects of type Liste a because every element must be listed and it is impossible to list an infinite number of elements.


tailfolde :: (b -> a -> b) -> b -> Liste a -> b
tailfolde c n Lin = n
tailfolde c n (Snoc a b) = tailfolde c (c n b) a


tailfold c n [] = n
tailfold c n (x:xs) = tailfold c (c n x) xs

list' :: Liste a -> [a]
list' = tailfolde (flip (:)) []

liste' :: [a] -> Liste a
liste' = tailfold (Snoc) Lin

unfold n h t x
    | n x = []
    | otherwise = h x : unfold n h t (t x)

unfolde :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> Liste b
unfolde n h t x
    | n x = Lin
    | otherwise = Snoc (unfolde n h t (t x)) (h x)

list'' :: Eq a => Liste a -> [a]
list'' = reverse.unfold (==Lin) (fsty) (sndy)

liste'' :: Eq a => [a] -> Liste a
liste'' xs = unfolde (==[]) (head) tail (reverse xs)

fsty (Snoc a b) = b
sndy (Snoc a b) = a
