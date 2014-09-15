{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- The UndecidableInstances flag is needed under 6.12.3 for the
-- HasStyle (a,b) instance.

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Style
-- Copyright   :  (c) 2011 diagrams-core team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A definition of /styles/ for diagrams as extensible, heterogeneous
-- collections of attributes.
--
-----------------------------------------------------------------------------

module Diagrams.Core.Style
       ( -- * Attributes
         -- $attr

         AttributeClass
       , Attribute(..)
       , mkAttr, mkTAttr, mkGTAttr, unwrapAttr
       , applyAttr, applyTAttr, applyGTAttr

         -- * Styles
         -- $style

       , Style(..)
       , attrToStyle, tAttrToStyle, gtAttrToStyle
       , getAttr, setAttr, addAttr, combineAttr
       , gmapAttrs

       , HasStyle(..)

       ) where

import           Control.Arrow           ((***))
import           Control.Lens            (Rewrapped, Wrapped (..), iso, (%~), (&))
import           Data.Data
import           Data.Data.Lens          (template)
import qualified Data.Map                as M
import           Data.Monoid.Action
import           Data.Semigroup
import qualified Data.Set                as S

import           Diagrams.Core.Transform
import           Diagrams.Core.V

------------------------------------------------------------
--  Attributes  --------------------------------------------
------------------------------------------------------------

-- $attr
-- An /attribute/ is anything that determines some aspect of a
-- diagram's rendering.  The standard diagrams library defines several
-- standard attributes (line color, line width, fill color, etc.) but
-- additional attributes may easily be created.  Additionally, a given
-- backend need not handle (or even know about) attributes used in
-- diagrams it renders.
--
-- The attribute code is inspired by xmonad's @Message@ type, which
-- was in turn based on ideas in:
--
-- Simon Marlow.
-- /An Extensible Dynamically-Typed Hierarchy of Exceptions/.
-- Proceedings of the 2006 ACM SIGPLAN workshop on
-- Haskell. <http://research.microsoft.com/apps/pubs/default.aspx?id=67968>.

-- | Every attribute must be an instance of @AttributeClass@, which
--   simply guarantees 'Typeable' and 'Semigroup' constraints.  The
--   'Semigroup' instance for an attribute determines how it will combine
--   with other attributes of the same type.
class (Typeable a, Semigroup a) => AttributeClass a

-- | An existential wrapper type to hold attributes.  Some attributes
--   are simply inert/static; some are affected by transformations;
--   and some are affected by transformations and can be modified
--   generically.
data Attribute (v :: * -> *) n :: * where
  Attribute   :: AttributeClass a => a -> Attribute v n
  TAttribute  :: (AttributeClass a, Transformable a, V a ~ v, N a ~ n) => a -> Attribute v n
  GTAttribute :: (AttributeClass a, Data a, Transformable a, V a ~ v, N a ~ n) => a -> Attribute v n


  -- Note: one could imagine requiring all attributes to be generic,
  -- but adding Data instances for everything would be a big pain in
  -- the butt, especially for things in other packages which don't
  -- export their constructors (e.g. FingerTree).  Having three
  -- different attribute wrappers is not ideal but it's far less work
  -- than the alternative.

type instance V (Attribute v n) = v
type instance N (Attribute v n) = n

-- | Wrap up an attribute.
mkAttr :: AttributeClass a => a -> Attribute v n
mkAttr = Attribute

-- | Wrap up a transformable attribute.
mkTAttr :: (AttributeClass a, Transformable a) => a -> Attribute (V a) (N a)
mkTAttr = TAttribute

-- | Wrap up a transformable and generic attribute.
mkGTAttr :: (AttributeClass a, Data a, Transformable a) => a -> Attribute (V a) (N a)
mkGTAttr = GTAttribute

-- | Unwrap an unknown 'Attribute' type, performing a dynamic (but
--   safe) check on the type of the result.  If the required type
--   matches the type of the attribute, the attribute value is
--   returned wrapped in @Just@; if the types do not match, @Nothing@
--   is returned.
unwrapAttr :: AttributeClass a => Attribute v n -> Maybe a
unwrapAttr (Attribute a)   = cast a
unwrapAttr (TAttribute a)  = cast a
unwrapAttr (GTAttribute a) = cast a

-- | Attributes form a semigroup, where the semigroup operation simply
--   returns the right-hand attribute when the types do not match, and
--   otherwise uses the semigroup operation specific to the (matching)
--   types.
instance Semigroup (Attribute v n) where
  (Attribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing  -> a2
      Just a2' -> Attribute (a1 <> a2')
  (TAttribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing  -> a2
      Just a2' -> TAttribute (a1 <> a2')
  (GTAttribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing -> a2
      Just a2' -> GTAttribute (a1 <> a2')

instance Transformable (Attribute v n) where
  transform _ (Attribute a)   = Attribute a
  transform t (TAttribute a)  = TAttribute (transform t a)
  transform t (GTAttribute a) = GTAttribute (transform t a)

------------------------------------------------------------
--  Styles  ------------------------------------------------
------------------------------------------------------------

-- $style
-- A 'Style' is a heterogeneous collection of attributes, containing
-- at most one attribute of any given type.  This is also based on
-- ideas stolen from xmonad, specifically xmonad's implementation of
-- user-extensible state.

-- | A @Style@ is a heterogeneous collection of attributes, containing
--   at most one attribute of any given type.
newtype Style v n = Style (M.Map String (Attribute v n))
  -- The String keys are serialized TypeRep values, corresponding to
  -- the type of the stored attribute.

instance Wrapped (Style v n) where
  type Unwrapped (Style v n) = M.Map String (Attribute v n)
  _Wrapped' = iso (\(Style m) -> m) Style

instance Rewrapped (Style v n) (Style v' n)

type instance V (Style v n) = v
type instance N (Style v n) = n

-- | Helper function for operating on styles.
inStyle :: (M.Map String (Attribute v n) -> M.Map String (Attribute v n))
        -> Style v n -> Style v n
inStyle f (Style s) = Style (f s)

-- | Extract an attribute from a style of a particular type.  If the
--   style contains an attribute of the requested type, it will be
--   returned wrapped in @Just@; otherwise, @Nothing@ is returned.
getAttr :: forall a v n. AttributeClass a => Style v n -> Maybe a
getAttr (Style s) = M.lookup ty s >>= unwrapAttr
  where ty = show . typeOf $ (undefined :: a)
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Create a style from a single attribute.
attrToStyle :: forall a v n. AttributeClass a => a -> Style v n
attrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkAttr a))

-- | Create a style from a single transformable attribute.
tAttrToStyle :: forall a. (AttributeClass a, Transformable a) => a -> Style (V a) (N a)
tAttrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkTAttr a))

-- | Create a style from a single transformable, generic attribute.
gtAttrToStyle :: forall a. (AttributeClass a, Data a, Transformable a) => a -> Style (V a) (N a)
gtAttrToStyle a = Style (M.singleton (show . typeOf $ (undefined :: a)) (mkGTAttr a))

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: forall a v n. AttributeClass a => a -> Style v n -> Style v n
setAttr a = inStyle $ M.insert (show . typeOf $ (undefined :: a)) (mkAttr a)

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: AttributeClass a => a -> Style v n -> Style v n
addAttr a s = attrToStyle a <> s

-- | Add a new attribute to a style that does not already contain an
--   attribute of this type, or combine it on the left with an existing
--   attribute.
combineAttr :: AttributeClass a => a -> Style v n -> Style v n
combineAttr a s =
  case getAttr s of
    Nothing -> setAttr a s
    Just a' -> setAttr (a <> a') s

-- | Map generically over all generic attributes in a style, applying
--   the given function to any values with the given type, even deeply
--   nested ones.  Note that only attributes wrapped in 'GTAttribute'
--   are affected.
gmapAttrs :: forall v a n. Typeable a => (a -> a) -> Style v n -> Style v n
gmapAttrs f = (inStyle . M.map) gmapAttr
  where
    gmapAttr :: Attribute v n -> Attribute v n
    gmapAttr (GTAttribute a) = GTAttribute (a & template %~ f)
    gmapAttr a = a

instance Semigroup (Style v n) where
  Style s1 <> Style s2 = Style $ M.unionWith (<>) s1 s2

-- | The empty style contains no attributes; composition of styles is
--   a union of attributes; if the two styles have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Monoid (Style v n) where
  mempty = Style M.empty
  mappend = (<>)

instance Num n => Transformable (Style v n) where
  transform t = inStyle $ M.map (transform t)

-- | Styles have no action on other monoids.
instance Action (Style v n) m

-- | Type class for things which have a style.
class HasStyle a where
  -- | /Apply/ a style by combining it (on the left) with the
  --   existing style.
  applyStyle :: Style (V a) (N a) -> a -> a

instance HasStyle (Style v n) where
  applyStyle = mappend

instance (HasStyle a, HasStyle b, Vn a ~ Vn b) => HasStyle (a,b) where
  applyStyle s = applyStyle s *** applyStyle s

instance HasStyle a => HasStyle [a] where
  applyStyle = fmap . applyStyle

instance HasStyle b => HasStyle (a -> b) where
  applyStyle = fmap . applyStyle

instance HasStyle a => HasStyle (M.Map k a) where
  applyStyle = fmap . applyStyle

instance (HasStyle a, Ord a) => HasStyle (S.Set a) where
  applyStyle = S.map . applyStyle

-- | Apply an attribute to an instance of 'HasStyle' (such as a
--   diagram or a style).  If the object already has an attribute of
--   the same type, the new attribute is combined on the left with the
--   existing attribute, according to their semigroup structure.
applyAttr :: (AttributeClass a, HasStyle d) => a -> d -> d
applyAttr = applyStyle . attrToStyle

-- | Apply a transformable attribute to an instance of 'HasStyle'
--   (such as a diagram or a style).  If the object already has an
--   attribute of the same type, the new attribute is combined on the
--   left with the existing attribute, according to their semigroup
--   structure.
applyTAttr :: (AttributeClass a, Transformable a, Vn a ~ Vn d, HasStyle d) => a -> d -> d
applyTAttr = applyStyle . tAttrToStyle

applyGTAttr :: (AttributeClass a, Data a, Transformable a, Vn a ~ Vn d, HasStyle d) => a -> d -> d
applyGTAttr = applyStyle . gtAttrToStyle

