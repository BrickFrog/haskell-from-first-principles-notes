
-- Identity
data Identity a =
    Identity a

instance Eq a => Eq (Identity a) where
    (==) (Identity v) (Identity v') = v == v'

-- Ex. 1
data TisAnInteger = 
    TisAn Integer 

instance Eq TisAnInteger where
    (==) (TisAn v) (TisAn v') = 
        compare v v' == EQ 

-- Ord Instances

data DayOfWeek = 
    Mon | Tue | Weds | Thu | Fri |Sat | Sun
    deriving (Eq, Show)

instance Ord DayOfWeek where
    compare Fri Fri = EQ 
    compare Fri _ = GT 
    compare _ Fri = LT 
    compare _ _ = EQ 


-- Ch 6 Exercises, Typechecking
-- 1) Was missing show, added a deriving
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2) Needed to derive Eq for equality check
-- 3) Blah/Woot. You need Ord to use > and 9 has no num instance
data Mood = Blah
            | Woot deriving (Show, Eq, Ord)

settleDown x = if x == Woot
                then Blah
                else x

-- 4) missing arguments for s1
type Subject = String 
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Matching the Types

--1) Num byitself needs typecheck, would work with Integer 
i :: Num a => a
i = 1


--2+3) replace with Num a => a doesn't work, needs fractional
f :: Float 
f = 1.0

f' :: Fractional a => a
f' = 1.0


-- Type - Kwon do Sampling
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f x y = f x == y