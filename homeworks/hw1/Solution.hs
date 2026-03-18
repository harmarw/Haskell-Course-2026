{-# LANGUAGE BangPatterns #-}

module Main where

-- Task 1
isPrimeBounded :: Int -> Bool
isPrimeBounded n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n =
  [ (p, q)
    | p <- [2 .. n `div` 2],
      let q = n - p,
      isPrimeBounded p,
      isPrimeBounded q
  ]

-- Task 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =
  [ (x, y)
    | x <- xs,
      y <- xs,
      x < y,
      gcd x y == 1
  ]

-- Task 3
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primesTo :: Int -> [Int]
primesTo n = sieve [2 .. n]

isPrime :: Int -> Bool
isPrime n
  | n < 2 = False
  | otherwise = n `elem` primesTo n

-- Task 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [ [ sum [(a !! i !! k) * (b !! k !! j) | k <- [0 .. p - 1]]
      | j <- [0 .. n - 1]
    ]
    | i <- [0 .. m - 1]
  ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- Task 5
permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations _ [] = []
permutations k xs =
  [ y : ys
    | (y, rest) <- select xs,
      ys <- permutations (k - 1) rest
  ]

select :: [a] -> [(a, [a])]
select [] = []
select (x : xs) = (x, xs) : [(y, x : ys) | (y, ys) <- select xs]

-- Task 6
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xa@(x : xs) ya@(y : ys)
  | x < y = x : merge xs ya
  | x > y = y : merge xa ys
  | otherwise = x : merge xs ys

hamming :: [Integer]
hamming =
  1
    : merge
      (map (2 *) hamming)
      ( merge
          (map (3 *) hamming)
          (map (5 *) hamming)
      )

-- Task 7
power :: Int -> Int -> Int
power b e = go 1 e
  where
    go !acc 0 = acc
    go !acc n = go (acc * b) (n - 1)

-- Task 8
-- seq
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "empty list"
listMaxSeq (x : xs) = go x xs
  where
    go acc [] = acc
    go acc (y : ys) =
      let acc' = max acc y
       in acc' `seq` go acc' ys

-- bang patterns
listMaxBang :: [Int] -> Int
listMaxBang [] = error "empty list"
listMaxBang (x : xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y : ys) = go (max acc y) ys

-- Task 9
primes :: [Int]
primes = sieve [2 ..]

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n
  | n < 2 = False
  | otherwise = go primes
  where
    go (p : ps)
      | p == n = True
      | p > n = False
      | otherwise = go ps
    go [] = False

-- Task 10
-- (a)
mean :: [Double] -> Double
mean xs =
  let (s, n) = go xs (0, 0)
   in s / fromIntegral n
  where
    go [] (s, n) = (s, n)
    go (y : ys) (s, n) = go ys (s + y, n + 1)

-- (b)
meanStrict :: [Double] -> Double
meanStrict xs =
  let (s, n) = go xs 0 0
   in s / fromIntegral n
  where
    go [] !s !n = (s, n)
    go (y : ys) !s !n = go ys (s + y) (n + 1)

-- (c)
meanVariance :: [Double] -> (Double, Double)
meanVariance xs =
  let (s, s2, n) = go xs 0 0 0
      meanVal = s / fromIntegral n
      varVal = s2 / fromIntegral n - meanVal * meanVal
   in (meanVal, varVal)
  where
    go [] !s !s2 !n = (s, s2, n)
    go (y : ys) !s !s2 !n = go ys (s + y) (s2 + y * y) (n + 1)

main :: IO ()
main = do
  putStrLn "1. Goldbach Pairs"
  print $ goldbachPairs 28
  putStrLn "\n2. Coprime Pairs"
  print $ coprimePairs [1 .. 10]

  putStrLn "\n3. Primes (Sieve)"
  print $ primesTo 50
  print $ isPrime 47
  print $ isPrime 48

  putStrLn "\n4. Matrix Multiplication"
  let a = [[1, 2, 3], [4, 5, 6]]
      b = [[7, 8], [9, 10], [11, 12]]
  print $ matMul a b

  putStrLn "\n5. Permutations"
  print $ permutations 2 [1, 2, 3]

  putStrLn "\n6. Hamming Numbers"
  print $ take 20 hamming

  putStrLn "\n7. Power"
  print $ power 2 10

  putStrLn "\n8. Running Maximum"
  print $ listMaxSeq [3, 1, 7, 2, 9, 4]
  print $ listMaxBang [3, 1, 7, 2, 9, 4]

  putStrLn "\n9. Infinite Primes"
  print $ take 20 primes
  print $ isPrimeInfinite 97
  print $ isPrimeInfinite 100

  putStrLn "\n10. Mean & Variance"
  let xs = [1 .. 10]
  print $ mean xs
  print $ meanStrict xs
  print $ meanVariance xs