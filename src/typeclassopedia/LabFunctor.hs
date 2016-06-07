module LabFunctor where

import Control.Applicative

-- Exercise 1: Functor instance for Either --

-- first define our own Either, to not clash with the default one
data CustomEither a b = This a | That b deriving (Eq, Ord, Read, Show)

instance Functor (CustomEither a) where
  fmap _ (This a) = This a
  fmap f (That b) = That (f b)

-- compiles but redundant
-- instance Functor ((->) e) where
--   fmap = (.)


-- Exercise 2: Functor instance for Pair and (a,b) --
data Pair a = Pair a a

instance Functor (Pair) where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- compiles but redundant
-- instance Functor ((,) a) where
--   fmap f (a, b) = (a, f b)

-- Similarities: they are both arity-two tuples, but one has one type parameter, the other two. mapping is only defined
-- over the type parameter bound by the Functor instance


-- Exercise 3: Functor instance ITree --

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf ia) = Leaf $ f . ia
  fmap f (Node tree) = Node $ fmap (fmap f) tree


-- Exercise 4: Example of a type of kind * -> * which cannot be made an instance of Functor --


-- Exercise 5: The composition of two Functors is also a Functor. Proof or didn't happen --
-- Useful information: http://stackoverflow.com/questions/19774564/what-does-it-mean-to-compose-two-functors
data Compose f g x = Compose (f (g x)) deriving (Show)

instance (Functor f1, Functor f2) => Functor (Compose f1 f2) where
  fmap f (Compose e) = Compose $ fmap (fmap f) e



-- === Functor Laws === --
-- fmap id = id
-- fmap (g . h) = (fmap g) . (fmap h)
-- these laws ensure that fmap g does not change the structure of a container, only the elements;

-- Exercise 1: Give an example of a (bogus) Functor instance which satisfies the second law but not the first

data BogusType x = Bottom | Bogus x deriving (Show)
instance Functor BogusType where
  fmap f _ = Bottom -- Respects the composition law because result will be Bottom regardless, but identity law is violated

-- Exercise 2: Which functor laws are violated by the evil Functor instance
-- -- Evil Functor instance
-- instance Functor [] where
--   fmap _ [] = []
--   fmap g (x:xs) = g x : g x : fmap g xs

-- identity law is violated: fmap id [x] = [x, x] =/= id
-- composition law is violated: fmap (id . id) [x] =/= (fmap id) . (fmap id) [x]


-- == Applicative Functor == --

-- LAWS
-- Identity law:    pure id <*> v = v
-- Homomorphism:    pure f <*> pure x = pure (f x)
-- Interchange:     u <*> pure y = pure ($ y) <*> u
-- Composition:     u <*> (v <*> w) = pure (.) <*> u <*> v <*> w

-- Proof exercise: Prove a variant of the interchange law: "pure f <*> x = pure (flip ($)) <*> x <*> pure f", using the laws

-- Proof:
--  1)  pure f <*> x = pure (flip ($)) <*> x <*> pure f\
--  2)  TODO

--- Exercise 1: Applicative instance for (custom) maybe
data CustomMaybe a = None | Some a

instance Functor CustomMaybe where
    fmap f None = None
    fmap f (Some a) = Some $ f a

instance Applicative CustomMaybe where
    pure =  Some
    (Some f) <*> maybeA = fmap f maybeA
    _ <*> _ = None

-- Exercise 2: Determine the correct definition of pure for the ZipList instance of Applicative

newtype ZipList2 a = ZipList2 { getZipList :: [a] }

instance Functor ZipList2 where
  fmap f (ZipList2 a) = ZipList2 $ fmap f a

instance Applicative ZipList2 where
  pure a = ZipList2 [a]
  (ZipList2 gs) <*> (ZipList2 xs) = ZipList2 (zipWith ($) gs xs)