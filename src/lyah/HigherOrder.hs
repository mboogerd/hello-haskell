module HigherOrder where

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


-- Infix functions can also be partially applied by using sections;
-- To section an infix function, simply surround it with parentheses and only supply a parameter on one side
-- Calling, say, divideByTen 200 is equivalent to doing 200 / 10, as is doing (/10) 200
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

tenDividedBy :: (Floating a) => a -> a
tenDividedBy = (10/)

--- === Some higher-orderism is in order === ---
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x


flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y


--- === Mapping and Filtering === ---

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0


--- Collatz numbers exercise ---
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
        | even n = n : collatz (n `div` 2)
        | odd n = n : collatz (n * 3 + 1)

collatzLength = length . collatz

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
    where isLong xs = length xs > 15


--- === Lambda's === ---
flip''' :: (a -> b -> c) -> b -> a -> c
flip''' f = \x y -> f y x


--- === Folds === ---

-- foldl/r1 requires a non-empty list; therefore doesn't need a starting zero value argument

maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- or alternatively
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


--- === Function application === ---

-- $ has the lowest precedence and is right-associative
-- you can imagine a $ being sort of the equivalent of writing an opening parentheses and then
-- writing a closing one on the far right side of the expression
-- $ means that function application can be treated just like another function.
-- That way, we can, for instance, map function application over a list of functions.

invertedMap :: a -> [a -> b] -> [b]
invertedMap a fList = map ($ a) fList


--- == Function composition === ---
-- If you want to rewrite an expression with a lot of parentheses by using function composition, you can start by
-- putting the last parameter of the innermost function after a $ and then just composing all the other function calls,
-- writing them without their last parameter and putting dots between them

-- Another common use of function composition is defining functions in the so-called point free style (also called the
-- pointless style). Take for example this function that we wrote earlier:

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs
--   ^                ^ appears in both expression on the right side, can therefore be eliminated as such:
sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0
