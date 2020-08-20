Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because

*Main> 19 * 402859
7654321

Repeatedly extracting tyhe smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]


Exercise 1:

factor 0 will return (2,0) because 0 `divMod` 2 will return (0,0), so r == 0 returns True and therefore factorFrom 2 0 returns (2,0).
factor 1 will enter an infinite loop because 1 `divMod`2 will return (0,2) so factorFrom will increment m to 3, and then get (0,3). This will keep happening with larger and larger m's, and the program will never terminate.


Exercise 2:

factor 0 = (2,0)
factor 1 = *interrupted*


Exercise 3:

Assume that the smallest factor of n is bigger than sqrt(n) and smaller than n. Then all other factors will also be larger than sqrt(n). Factors always come in pairs whose product is n. Therefore, there is some factor which, when multiplied by the smallest factor of n, gives the product n. As stated earlier, if all factors of n are bigger than sqrt(n), then the smallest factor of n multiplied by any other factor will be larger than sqrt(n) * sqrt(n), which will be larger than n. However, this is a contradiction as if there is no pair of factors whose product is n, then they are not factors of n. Therefore, the smallest factor of n must be less than or equal to sqrt(n).

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0     = (m,q)
>                 | n <= m * m = (n,1)
>                 | otherwise  = factorFrom1 (m+1) n
>    where (q,r) = n `divMod` m

The order of the guarded equations matters as if n is a perfect square and the program tests for whether n is less than or equal to m * m first, then n is equal to m * m, so it is caught by the guarded equation and returns (sqrt(n),1) rather than (sqrt(n),sqrt(n)).

In the worst case, n is a prime number and factor1 n tests all values of m from 2 to the first integer greater than sqrt(n). Therefore, in the worst case, the number of recursive calls made is sqrt(n) - 1.


Exercise 4:

Testing for whether q<m is the same as testing for n <= m*m because qm+r=n. If q is less than m, that means that the point where q=m has passed, and therefore m squared is larger than n. This is because q is r less than n/m, so q<n/m. Then qm<n, and since q<m, qm<m*m<=n, so testing for q<m is the same as testing for n <= m*m. However, it is more efficient because m*m does not have to be computed. As the test for n <= m*m is after the rest for r == 0, for which n `divMod` m is computed, (q,r) has already been calculated so a test which uses a value that is already there is better than a test for which something extra must be calculated.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0     = (m,q)
>                 | q < m      = (n,1)
>                 | otherwise  = factorFrom2 (m+1) n
>    where (q,r) = n `divMod` m


Exercise 5:

> factor3 :: Integer -> (Integer, Integer)
> factor3 n = factorFrom3 2 n

> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | r == 0     = (m,q)
>                 | m == 2     = factorFrom3 3 n
>                 | q < m      = (n,1)
>                 | otherwise  = factorFrom3 (m+2) n
>    where (q,r) = n `divMod` m

This version is about twice as efficient as factor2, as half of all numbers are even, so if factor3 only tests one even number and skips all others, it tests almost half of the amount of numbers that factor2 tests.


Exercise 6:

factor2 5 = (5,1)
(0.00 secs, 74,904 bytes)
factor3 5 = (5,1)
(0.00 secs, 74,888 bytes)

factor2 169 = (13,13)
(0.00 secs, 82,128 bytes)
factor3 169 = (13,13)
(0.00 secs, 80,864 bytes)

factor2 15485863 = (15485863,1)
  (0.01 secs, 1,686,056 bytes)
factor3 (15485863) = (15485863,1)
  (0.01 secs, 992,744 bytes)

factor2 217645177 = (217645177,1)
  (0.02 secs, 6,099,856 bytes)
factor3 217645177 = (217645177,1)
  (0.01 secs, 3,504,544 bytes)

factor2 982451653 = (982451653,1)
  (0.03 secs, 12,867,944 bytes)
factor3 982451653 = (982451653,1)
  (0.02 secs, 7,351,632 bytes)

factor2(2^64+1) = (274177,67280421310721)
  (0.22 secs, 149,239,736 bytes)
factor3(2^64+1) = (274177,67280421310721)
  (0.13 secs, 82,342,112 bytes)


Exercise 7:

> factor4 :: Integer -> (Integer, Integer)
> factor4 n = factorFrom4 2 n 2

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0     = (m,q)
>                   | m == 2     = factorFrom4 3 n 2
>                   | q < m      = (n,1)
>                   | otherwise  = factorFrom4 (m+s) n (6-s)
>    where (q,r) = n `divMod` m


Exercise 8:

Using only prime numbers as trial divisors requires either a list of all primes less than or equal to the square root of the largest number that can be factored with the program, or all numbers have to be tested for being prime, which means recursively calling on factor when trying to decide which numbers to use as divisors in factorsFrom. This worsens the efficiency as there is a lot of extra testing. This occurs because there is no real pattern for the distribution of prime numbers.


Exercise 9:

> factors2 :: Integer -> [Integer]
> factors2 n = factorsFrom 2 n

> factorsFrom2 :: Integer -> Integer -> [Integer]
> factorsFrom2 m n | n == 1    = []
>                 | otherwise = p:factorsFrom2 p q
>    where (p,q) = factorFrom4 m n 2


Exercise 10:

factors 182932 = [2,2,19,29,83]
  (0.00 secs, 112,832 bytes)
factors2 182932 = [2,2,19,29,83]
  (0.00 secs, 112,960 bytes)

factors 7813940248123 = [3359,5081,457837]
  (0.23 secs, 164,905,640 bytes)
factors2 7813940248123 = [3359,5081,457837]
  (0.23 secs, 164,903,768 bytes)

factors (2^32+1) = [641,6700417]
  (3.11 secs, 2,412,235,088 bytes)
factors2 (2^32+1) = [641,6700417]
  (3.05 secs, 2,412,233,296 bytes)

Jevon's Problem:
factors2 8616460799 = [89681,96079]
  (0.06 secs, 34,670,024 bytes)
