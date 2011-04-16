{-# LANGUAGE TypeOperators
           , MultiParamTypeClasses
           , FlexibleInstances
           , OverlappingInstances
           , UndecidableInstances
           , TypeFamilies
           , GeneralizedNewtypeDeriving
  #-}

-- XXX comment me
module Graphics.Rendering.Diagrams.MList where

import Data.Monoid
import Graphics.Rendering.Diagrams.Util
import Graphics.Rendering.Diagrams.Monoids

infixr 5 :::

data Nil     = Nil
data a ::: b = Missing b
             | a ::: b

-- MList -----------------------------------

class MList l where
  empty   :: l

instance MList Nil where
  empty     = Nil

instance MList l => MList (a ::: l) where
  empty   = Missing empty

-- Monoid ----------------------------------

instance Monoid Nil where
  mempty        = Nil
  _ `mappend` _ = Nil

instance (Monoid a, Monoid tl) => Monoid (a ::: tl) where
  mempty = Missing mempty
  (Missing t1) `mappend` (Missing t2) = Missing (t1 `mappend` t2)
  (Missing t1) `mappend` (a2 ::: t2)  = a2 ::: (t1 `mappend` t2)
  (a1 ::: t1)  `mappend` (Missing t2) = a1 ::: (t1 `mappend` t2)
  (a1 ::: t1)  `mappend` (a2 ::: t2)  = (a1 `mappend` a2) ::: (t1 `mappend` t2)

-- ToTuple ---------------------------------

type family Tuple l :: *
type instance Tuple Nil       = ()
type instance Tuple (a ::: b) = (a, Tuple b)

class ToTuple l where
  toTuple :: l -> Tuple l

instance ToTuple Nil where
  toTuple _ = ()

instance (Monoid a, ToTuple l) => ToTuple (a ::: l) where
  toTuple (Missing l) = (mempty, toTuple l)
  toTuple (a ::: l)   = (a, toTuple l)

-- Embedding -------------------------------------------

class l :>: a where
  inj  :: a -> l
  get  :: l -> a
  alt  :: (a -> a) -> l -> l

instance (MList t, Monoid a) => (:>:) (a ::: t) a where
  inj a                = a ::: empty
  get (Missing _)      = mempty
  get (a ::: _)        = a
  alt f (Missing l)    = f mempty ::: l
  alt f (a ::: l)      = f a ::: l

instance (t :>: a) => (:>:) (b ::: t) a where
  inj a                = Missing (inj a)
  get (Missing l)      = get l
  get (_ ::: l)        = get l
  alt f (Missing l)    = Missing (alt f l)
  alt f (a ::: l)      = a ::: alt f l

-- Monoid actions -----------------------------------------
-- XXX add comments

newtype SM m = SM m
  deriving (Monoid)

instance Action Nil l where
  act _ a = a

instance (Monoid a, Action (SM a) l2, Action l1 l2) => Action (a ::: l1) l2 where
  act (Missing l1) l2 = act l1 l2
  act (a ::: l1) l2   = act (SM a) (act l1 l2)

instance Monoid a => Action (SM a) Nil where
  act _ _ = Nil

instance (Action a a', Action (SM a) l) => Action (SM a) (a' ::: l) where
  act (SM a) (Missing l) = Missing (act (SM a) l)
  act (SM a) (a' ::: l)  = act a a' ::: act (SM a) l
