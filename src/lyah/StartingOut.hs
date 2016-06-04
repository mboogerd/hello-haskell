module StartingOut (
  doubleMe,
  doubleUs,
  doubleSmallNumber,
  listComprehension
) where


doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
                        then x
                        else x*2

-- We usually use ' to either denote a strict version of a function (one that isn't lazy) or a slightly modified version of a function or a variable
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

-- functions can't begin with uppercase letters
-- When a function doesn't take any parameters, we usually say it's a definition (or a name)
conanO'Brien = "It's a-me, Conan O'Brien!"

-- a list comprehension is composed of: one output function, at least one list-drawing-function, and any number of predicates
listComprehension = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50, x+y > 20]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
adjectiveNoun = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]


triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangle a b c = a^2 + b^2 == c^2
rightTriangles = [ (a, b, c) | (a, b, c) <- triangles, rightTriangle a b c ]