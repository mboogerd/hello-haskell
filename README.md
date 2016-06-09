# Hello Haskell 

My journey of learning Haskell.

## Setup

In a nutshell what is explained in more detail [here](http://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html), enhanced with my own experience.

### Installing
```bash
brew update
brew install haskell-stack
```

### Creating a project
```bash
PROJECTNAME=hello-haskell

# Create a project in your current working directory using stack's `simple` template
stack new $PROJECTNAME simple

# Setup GHC for your project
cd $PROJECTNAME
stack setup
```

### Try it out
```bash
# Try running the Haskell REPL
stack ghci
# :q to quit

# Build and execute app/Main.hs
stack exec

# Build and test using test/Spec.hs
stack test
```

### Configure your project

My early investigation shows that the following works:

1. library hs-source-dirs; make sure any source folders you create are included
2. library exposed-modules; make sure that modules you use in test or Main are exposed
3. any build-depends; make sure that libraries you use in `import` statements are included here


## Learning

1. A very gentle, playful introduction is [Learn you a Haskell](http://learnyouahaskell.com/chapters). I recommend reading at least the first 8 chapters before continuing elsewhere. Do as many exercises as you can
2. A great continuation is [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia). Do _all_ the exercises.
3. Read the papers; too hard? find a few videos or blogs on the subject that attempt to give a more light-hearted explanation. Rince and repeat

## Testing

The gist of writing robust software is clean decomposition of your problem in smaller problems, solving the smaller problems, and composing the solutions together. 
Solving smaller problems is easier, but testing whether each partial solution is well-behaved is essential. Property-based testing is a great tool for functionally written solutions.
The de-facto library for property-based testing in Haskell is [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html):

```bash
stack install QuickCheck

# Also, include QuickCheck in your test's build-depends
```

Now, without being a professor in mathematics, you too can give strong statistical evidence that your code behaves lawfully!

```haskell
import Prelude (($), (.), (+))
import Control.Applicative
import Test.QuickCheck

-- Some type
newtype ZipList2 a = ZipList2 { getZipList2 :: [a] } deriving (Show, Eq)

-- Typeclass instance definitions; Our ZipList is an Applicative Functor
instance Functor ZipList2 where
  fmap f (ZipList2 a) = ZipList2 $ fmap f a

instance Applicative ZipList2 where
  pure a = ZipList2 $ repeat a
  (ZipList2 gs) <*> (ZipList2 xs) = ZipList2 (zipWith ($) gs xs)

-- Make sure that arbitrary instances of ZipList can be generated
instance (Arbitrary a) => Arbitrary (ZipList2 a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList2 a

-- identity law should hold for all things that claim to be Applicative
law_applicative_identity_ziplist :: ZipList2 Int -> Bool
law_applicative_identity_ziplist v = (pure id <*> v) == v

main = quickCheck law_applicative_identity_ziplist
```

More starting info can be found [here](http://chromaticleaves.com/posts/generate-user-data-quickcheck.html)