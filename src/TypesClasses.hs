module TypesClasses where


addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z


factorial :: Integer -> Integer
factorial n = product [1..n]


circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- :t read
-- read :: Read a => String -> a
-- [function] :: [class constraint 1], ..., [class constraint m] => [ parameter 1 ] -> ... -> [ parameter n] -> [ result ]

-- :: can be used as a means of type coercion, in case the compiler cannot resolve ambiguity
-- read "5" :: Int

-- Helpful typeclasses: Show, Read; Enum, Bounded; Num, Integral, Floating