
-- Synctactic sugar for list construction
-- [1, 2, 3] ++ [4]
--(1 : 2 : 3 : []) ++ 4 : []

import Data.Char ( isUpper, toUpper )

myWords :: String -> [String]
myWords [] = []
myWords (' ':xs) = myWords xs
myWords xs = takeWhile (/= ' ') xs : myWords (dropWhile (/= ' ') xs)

filterByUpper :: String -> String
filterByUpper = filter isUpper

capitalizeFirstChar :: String -> String
capitalizeFirstChar "" = ""
capitalizeFirstChar (x:xs) = toUpper x : xs

shout :: String -> String
shout "" = ""
shout (x:xs) = toUpper x : shout xs

-- Alternative using map
shout' :: String -> String
shout' = map toUpper

capitalizeHead :: String -> Char
capitalizeHead = toUpper . head

-- Write your own standard functions

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (x==)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish [x] = x
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
  where go _ [] acc = acc
        go g [y] acc = case g y acc of
                            GT -> y
                            _ -> acc
        go g (y:ys) acc = case g y acc of
                               GT -> go g ys y
                               _ -> go g ys acc

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = go f xs x
  where go _ [] acc = acc
        go g [y] acc = case g y acc of
                            LT -> y
                            _ -> acc
        go g (y:ys) acc = case g y acc of
                               LT -> go g ys y
                               _ -> go g ys acc

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare