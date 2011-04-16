{-# LANGUAGE ScopedTypeVariables
           , GADTs
           , KindSignatures
           , FlexibleInstances
           , MultiParamTypeClasses
  #-}

-- XXX comment
module Graphics.Rendering.Diagrams.Style
       ( -- * Attributes

         AttributeClass(..)
       , Attribute(..)
       , mkAttr, unwrapAttr
       , applyAttr


         -- * Styles

       , Style(..), inStyle
       , getAttr, setAttr, addAttr

       , HasStyle(..)

       , attrToStyle

       ) where

import Graphics.Rendering.Diagrams.Monoids
import Graphics.Rendering.Diagrams.Util

import Data.Typeable

import Data.Monoid
import qualified Data.Map as M

------------------------------------------------------------
--  Attributes  --------------------------------------------
------------------------------------------------------------

-- The attribute code is inspired by xmonad's Message type, which
-- was in turn based on ideas in /An Extensible Dynamically-Typed
-- Hierarchy of Exceptions/, Simon Marlow, 2006.

-- | Every attribute must be an instance of @AttributeClass@.
class Typeable a => AttributeClass a where

-- | An existential wrapper type to hold attributes.
data Attribute :: * where
  Attribute :: AttributeClass a => a -> Attribute

-- TODO: add to export lists etc.

-- | Create an attribute.
mkAttr :: AttributeClass a => a -> Attribute
mkAttr = Attribute

-- | Unwrap an unknown 'Attribute' type, performing a (dynamic) type
--   check on the result.
unwrapAttr :: AttributeClass a => Attribute -> Maybe a
unwrapAttr (Attribute a) = cast a

------------------------------------------------------------
--  Styles  ------------------------------------------------
------------------------------------------------------------

-- | A @Style@ is a collection of attributes, containing at most one
--   attribute of any given type.
newtype Style = Style (M.Map String Attribute)
  -- The String keys are serialized TypeRep values, corresponding to
  -- the type of the stored attribute.

inStyle :: (M.Map String Attribute -> M.Map String Attribute)
        -> Style -> Style
inStyle f (Style s) = Style (f s)

-- | Extract an attribute from a style using the magic of type
--   inference and "Data.Typeable".
getAttr :: forall a. AttributeClass a => Style -> Maybe a
getAttr (Style s) = M.lookup ty s >>= unwrapAttr
  where ty = (show . typeOf $ (undefined :: a))
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: forall a. AttributeClass a => a -> Style -> Style
setAttr a = inStyle $ M.insert (show . typeOf $ (undefined :: a)) (mkAttr a)

-- | The empty style contains no attributes; composition of styles is
--   right-biased union; i.e. if the two styles contain attributes of
--   the same type, the one from the right is taken.
instance Monoid Style where
  mempty = Style M.empty
  (Style s1) `mappend` (Style s2) = Style $ s2 `M.union` s1

-- | Styles have no action on other monoids.
instance Action Style m

-- | Create a style from a single attribute.
attrToStyle :: forall a. AttributeClass a => a -> Style
attrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkAttr a))

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: AttributeClass a => a -> Style -> Style
addAttr a s = attrToStyle a <> s

-- | Type class for things which have a style.
class HasStyle a where
  applyStyle :: Style -> a -> a  -- ^ /Apply/ a style by combining it
                                 -- (on the left) with the existing
                                 -- style.

instance HasStyle Style where
  applyStyle = mappend

-- | Apply an attribute to a diagram.  Note that child attributes
--   always have precedence over parent attributes.
applyAttr :: (AttributeClass a, HasStyle d) => a -> d -> d
applyAttr = applyStyle . attrToStyle

