module Modules where

import Data.List
import qualified Data.Map as M

intersperseTest :: [Char]
intersperseTest = intersperse '.' "Monkey"

intercalateTest :: [Char]
intercalateTest = intercalate " " ["hey","there","guys"]

transposeTest :: [[Int]]
transposeTest = transpose [[1,2,3],[4,5,6],[7,8,9]]

-- foldl' and foldl1' are stricter versions of their respective lazy incarnations.

-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat.
concatMapTest :: [Int]
concatMapTest = concatMap (replicate 4) [1..3]

-- and, or; test whether there exists some group for which all elements are >= 4
andOrTest :: Bool
andOrTest = or $ map (and . map (>=4)) [[1,2,3,4], [6,7,8,9]]

-- any, all; test whether there exists a group for which not all elements are >= 4
anyAllTest :: Bool
anyAllTest = any (== False) $ map (all (>=4)) [[1,2,3,4], [6,7,8,9]]

-- iterate, splittest
iterateTest :: Bool
iterateTest = iterate (++ "haha") "haha"

