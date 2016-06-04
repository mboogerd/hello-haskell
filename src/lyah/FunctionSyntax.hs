module FunctionSyntax where


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

-- or better:
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


-- product3 element accessors
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

-- if you want to bind to several variables (even if one of them is just _ and doesn't actually bind at all), we have to surround them in parentheses
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x


tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- === Guards, Guards === ---

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)
          -- names are aligned at a single column. If we don't align them nice and proper,
          -- Haskell gets confused because then it doesn't know they're all part of the same block.


max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

-- Not only can we call functions as infix with backticks, we can also define them using backticks. Sometimes it's easier to read that way.
myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT


initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- === Let it be === ---

-- The form is: let <bindings> in <expression>
-- The names that you define in the let part are accessible to the expression after the in part
-- let bindings are expressions themselves. where bindings are just syntactic constructs
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


letSquares = [let square x = x * x in (square 5, square 3, square 2)]

letInlineVariables = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

letPatternMatch = (let (a,b,c) = (1,2,3) in a+b+c) * 100

-- let inside a list comprehension does not require 'in' expresion; names defined in a let inside a list comprehension
-- are visible to the output function (the part before the |) and all predicates and sections that come after of the binding
-- i.e. We can't use the bmi name in the (w, h) <- xs part because it's defined prior to the let binding.
letCalcBmis :: (RealFloat a) => [(a, a)] -> [a]
letCalcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]


-- The in part can also be omitted when defining functions and constants directly in GHCi.
-- If we do that, then the names will be visible throughout the entire interactive session.


--- === Case Expressions === ---

--- case expression of pattern -> result
--                     pattern -> result
--                     pattern -> result
--                     ...

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

-- Whereas pattern matching on function parameters can only be done when defining functions, case expressions can be used pretty much anywhere
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."