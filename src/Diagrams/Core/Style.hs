{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

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
       , Attribute(..), _Attribute, _MAttribute, _TAttribute
       , mkAttr, mkMAttr, mkTAttr, unwrapAttr
       , applyAttr, applyMAttr, applyTAttr

         -- * Styles
         -- $style

       , Style(..)
       , attrToStyle, tAttrToStyle, mAttrToStyle
       , getAttr, setAttr, addAttr, combineAttr

       , unmeasureAttr, unmeasureAttrs

       , HasStyle(..)

       ) where

import           Control.Arrow           ((***))
import           Control.Lens            hiding (Action, transform)-- (Rewrapped, Wrapped (..), iso, (%~), (&))
import           Data.Typeable
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map                as M
import           Data.Monoid.Action
import           Data.Semigroup
import qualified Data.Set                as S

import           Diagrams.Core.Transform
import           Diagrams.Core.V
import           Diagrams.Core.Measure

import           Linear.Vector

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
  MAttribute  :: AttributeClass a => Measured n a -> Attribute v n
  TAttribute  :: (AttributeClass a, Transformable a, V a ~ v, N a ~ n) => a -> Attribute v n

-- | Prism onto 'Attribute'.
_Attribute :: AttributeClass a => Prism' (Attribute v n) a
_Attribute = prism' Attribute (\(Attribute a) -> cast a)

-- | Prism onto 'MAttribute'.
_MAttribute :: (AttributeClass a, Typeable n) => Prism' (Attribute v n) (Measured n a)
_MAttribute = prism' MAttribute (\(MAttribute a) -> cast a)

-- | Prism onto 'TAttribute'.
_TAttribute :: (AttributeClass a, Transformable a, V a ~ v, N a ~ n) => Prism' (Attribute v n) a
_TAttribute = prism' TAttribute (\(TAttribute a) -> cast a)

type instance V (Attribute v n) = v
type instance N (Attribute v n) = n

-- | Wrap up an attribute.
mkAttr :: AttributeClass a => a -> Attribute v n
mkAttr = Attribute

-- | Wrap up a measured attribute.
mkMAttr :: AttributeClass a => Measured n a -> Attribute v n
mkMAttr = MAttribute

-- | Wrap up a transformable attribute.
mkTAttr :: (AttributeClass a, Transformable a) => a -> Attribute (V a) (N a)
mkTAttr = TAttribute

-- | Unwrap an unknown 'Attribute' type, performing a dynamic (but
--   safe) check on the type of the result.  If the required type
--   matches the type of the attribute, the attribute value is
--   returned wrapped in @Just@; if the types do not match, @Nothing@
--   is returned.
unwrapAttr :: AttributeClass a => Attribute v n -> Maybe a
unwrapAttr (Attribute a)   = cast a
unwrapAttr (MAttribute _)  = Nothing -- can't unwarp measured attributes
unwrapAttr (TAttribute a)  = cast a
-- Measured is intentionally not an instance on 'AttributeClass' to avoid any 
-- mix ups.

-- | Same as 'unwrapAttr' but for an 'MAttribute'.
unwrapMAttr :: (AttributeClass a, Typeable n) => Attribute v n -> Maybe (Measured n a)
unwrapMAttr (MAttribute m) = cast m
unwrapMAttr _              = Nothing

-- | Attributes form a semigroup, where the semigroup operation simply
--   returns the right-hand attribute when the types do not match, and
--   otherwise uses the semigroup operation specific to the (matching)
--   types.
instance Typeable n => Semigroup (Attribute v n) where
  (Attribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing  -> a2
      Just a2' -> Attribute (a1 <> a2')
  (MAttribute (Measured a1)) <> a2 =
    case unwrapMAttr a2 of
      Just (Measured a2') -> MAttribute $ Measured (a1 <> a2')
      Nothing             -> a2
  (TAttribute a1) <> a2 =
    case unwrapAttr a2 of
      Nothing  -> a2
      Just a2' -> TAttribute (a1 <> a2')

instance (Additive v, Traversable v, Floating n) => Transformable (Attribute v n) where
  transform _ (Attribute a)   = Attribute a
  transform t (MAttribute a)  = MAttribute $ scaleLocal (avgScale t) a
  transform t (TAttribute a)  = TAttribute (transform t a)

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
newtype Style v n = Style (HM.HashMap TypeRep (Attribute v n))

type instance V (Style v n) = v
type instance N (Style v n) = n

instance Rewrapped (Style v n) (Style v' n')
instance Wrapped (Style v n) where
  type Unwrapped (Style v n) = HM.HashMap TypeRep (Attribute v n)
  _Wrapped' = iso (\(Style m) -> m) Style
  {-# INLINE _Wrapped' #-}

type instance Index (Style v n)   = TypeRep
type instance IxValue (Style v n) = Attribute v n

instance Ixed (Style v n) where
  ix k = _Wrapped' . ix k
  {-# INLINE ix #-}

instance At (Style v n) where
  at k = _Wrapped' . at k
  {-# INLINE at #-}

instance Each (Style v n) (Style v' n') (Attribute v n) (Attribute v' n') where
  each = _Wrapped . each
  {-# INLINE each #-}

-- | Map the attributes of a style, with the possibility of changing the space 
--   / number type.
attrMap :: (Attribute v n -> Attribute u n') -> Style v n -> Style u n'
attrMap f (Style s) = Style $ HM.map f s

-- | Helper function for operating on styles.
inStyle :: (HM.HashMap TypeRep (Attribute v n) -> HM.HashMap TypeRep (Attribute v n))
        -> Style v n -> Style v n
inStyle f (Style s) = Style (f s)

-- | Extract an attribute from a style of a particular type.  If the
--   style contains an attribute of the requested type, it will be
--   returned wrapped in @Just@; otherwise, @Nothing@ is returned.
getAttr :: forall a v n. AttributeClass a => Style v n -> Maybe a
getAttr (Style s) = HM.lookup ty s >>= unwrapAttr
  where ty = typeOf (undefined :: a)
  -- the unwrapAttr should never fail, since we maintain the invariant
  -- that attributes of type T are always stored with the key "T".

-- | Create a style from a single attribute.
attrToStyle :: AttributeClass a => a -> Style v n
attrToStyle a = Style (HM.singleton (typeOf a) (mkAttr a))

-- | Create a style from a single attribute.
mAttrToStyle :: forall v n a. (AttributeClass a, Typeable n) => Measured n a -> Style v n
mAttrToStyle a = Style (HM.singleton (typeOf (undefined :: a)) (mkMAttr a))
-- Note that we use type 'a' not 'Measured n a' so we don't have to rebuild 
-- when un-measuring the attributes.

-- | Create a style from a single transformable attribute.
tAttrToStyle :: (AttributeClass a, Transformable a) => a -> Style (V a) (N a)
tAttrToStyle a = Style (HM.singleton (typeOf a) (mkTAttr a))

-- | Add a new attribute to a style, or replace the old attribute of
--   the same type if one exists.
setAttr :: AttributeClass a => a -> Style v n -> Style v n
setAttr a = inStyle $ HM.insert (typeOf a) (mkAttr a)

-- | Attempt to add a new attribute to a style, but if an attribute of
--   the same type already exists, do not replace it.
addAttr :: (AttributeClass a, Typeable n) => a -> Style v n -> Style v n
addAttr a s = attrToStyle a <> s

-- | Add a new attribute to a style that does not already contain an
--   attribute of this type, or combine it on the left with an existing
--   attribute.
combineAttr :: forall a v n. (AttributeClass a, Typeable n) => a -> Style v n -> Style v n
combineAttr a = inStyle $ HM.insertWith (<>) (typeOf a) (mkAttr a)

unmeasureAttrs :: (Num n, Typeable n) => n -> n -> Style v n -> Style v n
unmeasureAttrs g n = attrMap (unmeasureAttr g n)
-- Note that measured attributes are stored with their type, not their measured 
-- type, so there's no need to rebuild the whole map to rename them

-- | Turn a 'MAttribute' into a 'Attribute'.
unmeasureAttr :: (Num n, Typeable n) => n -> n -> Attribute v n -> Attribute v n
unmeasureAttr g n (MAttribute m) = Attribute (fromMeasured g n m)
unmeasureAttr _ _ a              = a

instance Typeable n => Semigroup (Style v n) where
  Style s1 <> Style s2 = Style $ HM.unionWith (<>) s1 s2

-- | The empty style contains no attributes; composition of styles is
--   a union of attributes; if the two styles have attributes of the
--   same type they are combined according to their semigroup
--   structure.
instance Typeable n => Monoid (Style v n) where
  mempty = Style HM.empty
  mappend = (<>)

instance (Additive v, Traversable v, Floating n) => Transformable (Style v n) where
  transform t = attrMap (transform t)

-- | Styles have no action on other monoids.
instance Action (Style v n) m

-- | Type class for things which have a style.
class HasStyle a where
  -- | /Apply/ a style by combining it (on the left) with the
  --   existing style.
  applyStyle :: Style (V a) (N a) -> a -> a

instance Typeable n => HasStyle (Style v n) where
  applyStyle = mappend

instance (HasStyle a, HasStyle b, V a ~ V b, N a ~ N b) => HasStyle (a,b) where
  applyStyle s = applyStyle s *** applyStyle s

instance HasStyle a => HasStyle [a] where
  applyStyle = fmap . applyStyle

instance HasStyle b => HasStyle (a -> b) where
  applyStyle = fmap . applyStyle

instance HasStyle a => HasStyle (M.Map k a) where
  applyStyle = fmap . applyStyle

instance (HasStyle a, Ord a) => HasStyle (S.Set a) where
  applyStyle = S.map . applyStyle

instance HasStyle b => HasStyle (Measured n b) where
  applyStyle = fmap . applyStyle

-- | Apply an attribute to an instance of 'HasStyle' (such as a
--   diagram or a style).  If the object already has an attribute of
--   the same type, the new attribute is combined on the left with the
--   existing attribute, according to their semigroup structure.
applyAttr :: (AttributeClass a, HasStyle d) => a -> d -> d
applyAttr = applyStyle . attrToStyle

applyMAttr :: (N d ~ n, AttributeClass a, HasStyle d, Typeable n) => Measured n a -> d -> d
applyMAttr = applyStyle . mAttrToStyle

-- | Apply a transformable attribute to an instance of 'HasStyle'
--   (such as a diagram or a style).  If the object already has an
--   attribute of the same type, the new attribute is combined on the
--   left with the existing attribute, according to their semigroup
--   structure.
applyTAttr :: (AttributeClass a, Transformable a, V a ~ V d, N a ~ N d, HasStyle d) => a -> d -> d
applyTAttr = applyStyle . tAttrToStyle

