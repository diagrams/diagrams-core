{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
  #-}

module Graphics.Rendering.Diagrams.MList where

import Data.Monoid
import Graphics.Rendering.Diagrams.Util
import Graphics.Rendering.Diagrams.Monoids

data Nil     = Nil
data a ::: b = Missing b
             | a ::: b

class MList l where
  empty :: l

instance MList Nil where
  empty = Nil

instance MList l => MList (a ::: l) where
  empty = Missing empty

instance Monoid Nil where
  mempty        = Nil
  _ `mappend` _ = Nil

instance (Monoid a, Monoid tl) => Monoid (a ::: tl) where
  mempty = Missing mempty
  (Missing t1) `mappend` (Missing t2) = Missing (t1 `mappend` t2)
  (Missing t1) `mappend` (a2 ::: t2)  = a2 ::: (t1 `mappend` t2)
  (a1 ::: t1)  `mappend` (Missing t2) = a1 ::: (t1 `mappend` t2)
  (a1 ::: t1)  `mappend` (a2 ::: t2)  = (a1 `mappend` a2) ::: (t1 `mappend` t2)

class l :>: a where
  inj  :: a -> l
  get  :: l -> a

instance (MList t, Monoid a) => (:>:) (a ::: t) a where
  inj a                = a ::: empty
  get (Missing _)      = mempty
  get (a ::: _)        = a

instance (t :>: a) => (:>:) (b ::: t) a where
  inj a                = Missing (inj a)
  get (Missing tl)     = get tl
  get (_ ::: tl)       = get tl

{-
  applyL a (Missing tl) = a ::: tl
  applyL a (a' ::: tl)  = (a `mappend` a') ::: tl

  applyL a (Missing tl) = Missing (applyL a tl)
  applyL a (b ::: tl)   = b ::: (applyL a tl)
-}

instance (Monoid m, Monoid l, l :>: m) => Action m l where
  apply m = (inj m <>)