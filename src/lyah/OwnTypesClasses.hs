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

import qualified Data.Map as M

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



data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case M.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = M.fromList
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

-- === Recursive data structures === --

-- fixity declaration; *'s fixity is infixl 7 * and +'s fixity is infixl 6; both left-associative, * binds stronger than +
infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5  .++
(.++) :: List a -> List a -> List a
Empty .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- pattern matching is actually about matching constructors


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- checks whether the value of the first argument is contained in the tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right

treeInsertAll :: (Ord a) => Tree a -> [a] -> Tree a
treeInsertAll = foldr treeInsert

treeNumbers = [8,6,4,1,7,3,5]
numericTree = treeInsertAll EmptyTree treeNumbers
tenNotInTree = 10 `treeElem` numericTree == False
sevenInTree = 7 `treeElem` numericTree


-- === Typeclasses 102 === --

--  class is for defining new typeclasses and instance is for making our types instances

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- Because == (in Eq class) is defined in terms of /= and vice versa, we only had to overwrite one of them
-- in the instance declaration to create a "minimal complete definition"

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


-- Self-assigned exercise: Create an ad hoc (join-semi-lattice-like) class and a parameterized instance for inclusive-or
infixl 6  ***
class LatticeLike a where
  (***) :: a -> a -> a -- least-upperbound

data Ior a b = This a | That b | Both a b deriving (Show)

instance (Ord a, Ord b) => LatticeLike (Ior a b) where
  This a1 *** This a2 = This $ max a1 a2
  That b1 *** That b2 = That $ max b1 b2
  Both a1 b1 *** Both a2 b2 = Both (max a1 a2) (max b1 b2)
  This a *** That b = Both a b
  This a1 *** Both a2 b = Both (max a1 a2) b
  That b1 *** Both a b2 = Both a (max b1 b2)
  first *** second = second *** first -- three more cases to be exhaustive, but they are just mirrored


-- === Yes-No typeclass === --
class YesNo a where
    yesno :: a -> Bool

-- emulating more flexible boolean semantics: values that represent non-emptiness evaluate to true, otherwise false

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where -- this also includes Strings of course
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- mimick if functionality using YesNo
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


-- === Functor Typeclass === --
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


instance Functor (Ior a) where
    fmap f (That b) = That $ f b
    fmap f (Both a b) = Both a $ f b
    fmap f (This a) = This a

-- If we use fmap (\a -> a) (the identity function, which just returns its parameter) over some list, we expect to get back the same list as a result


-- === Kinds and some type-foo === --

-- Types are little labels that values carry so that we can reason about the values.
-- But types have their own little labels, called kinds; A kind is more or less the type of a type

-- A * means that the type is a concrete type
-- A concrete type is a type that doesn't take any type parameters and values can only have types that are concrete types

-- We use :k on a type to get its kind, just like we can use :t on a value to get its type

-- Type constructors are curried (just like functions), so we can partially apply them
-- ghci> :k Either String
-- Either String :: * -> *
-- ghci> :k Either String Int
-- Either String Int :: *

class Tofu t where
    tofu :: j a -> t a j

data Frank a b  = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x


data Barry t k p = Barry { yabba :: p, dabba :: t k } deriving (Show)

instance Functor (Barry t k) where
  fmap f (Barry { yabba = p, dabba = q }) = Barry { yabba = f p, dabba = q }