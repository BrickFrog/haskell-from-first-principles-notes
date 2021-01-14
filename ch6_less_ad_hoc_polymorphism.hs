
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

-- 2)
data Mood = Blah
            | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                then Blah
                else x