-- Simple recursion, factorial and base cases

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n -1)

-- fibonacci

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = 
    fibonacci (x -1) + fibonacci (x -2)

-- go function

data DividedResult
  = Result (Integer, Integer)
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy 0 _ = Result (0, 0)
dividedBy _ 0 = DividedByZero
dividedBy num denom = go absNum absDenom 0
  where multiplier = signum num * signum denom
        absNum = abs num
        absDenom = abs denom
        go n d count
          | n < d = Result (count * multiplier, n)
          | otherwise = go (n - d) d (count + 1)

multipliedBy :: Integral a => a -> a -> a
multipliedBy x y = go x 0 0
  where go x' s count
          | count == y = s
          | otherwise = go x' (s + x') (count + 1)