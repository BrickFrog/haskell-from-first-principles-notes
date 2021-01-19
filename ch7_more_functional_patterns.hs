-- Chapter 7 - More Functional Patterns

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

-- Pattern Matching on Tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- Exercises 1)
k :: (a, b) -> a
k (x, y) = x

k1 :: Integer
k1 = k ((4 -1), 10) --3

k2 :: [Char]
k2 = k ("three", (1 + 2)) --"three"

k3 :: Integer
k3 = k (3, True) --3

-- 2) same syntax for type / data constructors
f' ::
  (a, b, c) ->
  (d, e, f) ->
  ((a, d), (c, f))
f' (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case Practice Exercise
functionC x y = if (x > y) then x else y

functionC' x y =
  case x > y of
    True -> x
    False -> y

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- associativity of types

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

-- Artful Dodgy

dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

-- Initially 11
oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

-- Initially 21
oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- Guards

numbers :: (Ord a, Num a, Num p) => a -> p
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- Function composition example
--let f x = take 5 . enumFrom $ x

-- Exercise, Code
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Read a, Show a) => a -> a
roundTripPF a = (read . show) $ a