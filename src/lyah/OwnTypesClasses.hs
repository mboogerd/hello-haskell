module OwnTypesClasses
    ( Point(..)
    -- We could also opt not to export any value constructors for Shape by just writing Shape in the export statement.
    -- That way, someone importing our module could only make shapes by using the auxilliary functions baseCircle and baseRect.
    -- Data.Map uses that approach
    , Shape(..)
    , surface
    , nudge
    , baseCircle
    , baseRect
    ) where

import Data.Map as M

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)


surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- === Record syntax === --
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- By using record syntax to create this data type, Haskell automatically made these functions: firstName, lastName, age, height, phoneNumber and flavor

-- When making an instance of a record, we don't have to necessarily put the fields in the proper order,
-- as long as we list all of them. But if we don't use record syntax, we have to specify them in order.

data Car = Car String String Int deriving (Show)
carExample = Car "Ford" "Mustang" 1967 -- Does not use record syntax, all assigned fields must be in definition order

data Car2 a b c = Car2 { company :: a
                        , model :: b
                        , year :: c
                        } deriving (Show)
carExample2 = Car2 { model = "Mustang", company = "Ford", year = 1967} -- record syntax allows deviating from definition order


-- === Type parameters === --

--  it's a very strong convention in Haskell to never add typeclass constraints in data declarations

-- When declaring a data type, the part before the = is the type constructor
-- and the constructors after it (possibly separated by |'s) are value constructors
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n

vectorOperationTest = Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)


-- === Derived instances === --

-- Haskell can automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read
data Human = Human { fName :: String
                     , lName :: String
                     } deriving (Eq, Show, Read)

showTest :: Human -> [Char]
showTest h = show h

readTest :: [Char] -> Human
readTest s = read s -- "Human {fName = \"Merlijn\", lName = \"Boogerd\"}"

-- If we compare two values of the same type that were made using different constructors,
-- the value which was made with a constructor that's defined first is considered smaller

--data Bool = False | True deriving (Ord)

boolTest :: Bool
boolTest = True `compare` False == GT


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

dayTest :: Bool
dayTest = [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] == ([minBound .. maxBound] :: [Day])



-- === Type synonyms === --

-- Type synonyms (and types generally) can only be used in the type portion of Haskell.

-- We introduce type synonyms either to describe what some existing type represents in our functions (and thus our
-- type declarations become better documentation) or when something has a long-ish type that's repeated a lot
-- (like [(String,String)]) but represents something more specific in the context of our functions

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- type aliases can also take type parameters
type AssocList k v = [(k,v)]

-- we can partially apply type parameters and get new type constructors from them
type IntMap = M.Map Int -- supplies a key type-parameter but misses the value type-parameter

