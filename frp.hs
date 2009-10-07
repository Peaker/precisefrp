{-# LANGUAGE GADTs, KindSignatures #-}

import Prelude hiding (fst, snd, id)
import Control.Category(Category(..), (>>>))
import Control.Arrow(Arrow(..))

type Source a = SF () a
-- type Sink a = SF a ()

-- SF is a signal function from a to b.
-- It is either a primitive or composed from SF's sequentially or pa

data SF :: * -> * -> * where
  SequentialCompose :: SF input middle -> SF middle output -> SF input output
  ParallelCompose :: SF ai ao -> SF bi bo -> SF (ai, bi) (ao, bo)
  Identity :: SF a a
  Fst :: SF (a, b) a
  Snd :: SF (a, b) b
  Split :: SF a (a, a)
  Scanl :: s -> (s -> a -> s) -> SF a s

  -- blackbox: Can't determine what subset of its input it uses:
  PureFunction :: (a -> b) -> SF a b

  IOInput :: IO a -> -- ^ a blocking action that yields the next input
             Source a

swap :: SF (a, b) (b, a)
swap = Snd &&& Fst

-- so we can compose SF's together, and so we have first/second for free
instance Category SF where
  id = Identity
  x . y = SequentialCompose y x

instance Arrow SF where
  arr = PureFunction
  (***) = ParallelCompose
  f &&& g = split >>> (f *** g)
  -- (&&&) = (result . result) (split >>>) (***)
  first = (*** id)
  second = (id ***)

split :: SF a (a, a)   -- subset of input changes, we want to duplicate knowledge of which subset, for (&&&)
split = Split

fst :: SF (a, b) a
fst = Fst

snd :: SF (a, b) b
snd = Snd

scanlE :: s -> -- ^ initial state
          (s -> a -> s) -> -- ^ state and new value to new state
          SF a s
scanlE = Scanl

execute :: SF () () -> IO ()
execute = do
  

-- Arrow problems:
-- 1. Build-up and unpacking of tuples
-- 2. Over-computation: (a, b) only a changes, stuff that read b also get recomputed <-- solvable
