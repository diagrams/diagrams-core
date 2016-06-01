{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-unused-imports       #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Core.Style
-- Copyright   :  (c) 2011-2015 diagrams-core team (see LICENSE)
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

    -- ** Attributes prisms
  , _Attribute
  , _MAttribute
  , _TAttribute

    -- ** Attributes utilities
  , unwrapAttribute
  , unmeasureAttribute
  , attributeType

    -- * Styles
    -- $style

  , Style(..)

    -- ** Making styles
  , attributeToStyle

    -- ** Extracting attibutes from styles
  , getAttr
  , unmeasureAttrs

    -- ** Attibute lenses
  , atAttr
  , atMAttr
  , atTAttr

    -- ** Applying styles
  , applyAttr
  , applyMAttr
  , applyTAttr

  , HasStyle(..)

  ) where

import           Control.Applicative
import           Control.Arrow           ((***))
import           Control.Lens            hiding (transform)
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map                as M
import           Data.Monoid.Action      as A
import           Data.Semigroup
import qualified Data.Set                as S
import           Data.Typeable

import           Diagrams.Core.Measure
import           Diagrams.Core.Transform
import           Diagrams.Core.V

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
  Attribute  :: AttributeClass a => a -> Attribute v n
  MAttribute :: AttributeClass a => Measured n a -> Attribute v n
  TAttribute :: (AttributeClass a, Transformable a, V a ~ v, N a ~ n) => a -> Attribute v n

type instance V (Attribute v n) = v
type instance N (Attribute v n) = n

-- | Attributes form a semigroup, where the semigroup operation simply
--   returns the right-hand attribute when the types do not match, and
--   otherwise uses the semigroup operation specific to the (matching)
--   types.
instance Typeable n => Semigroup (Attribute v n) where
  (Attribute a1)  <> (preview _Attribute  -> Just a2) = Attribute  (a1 <> a2)
  (MAttribute a1) <> (preview _MAttribute -> Just a2) = MAttribute (a1 <> a2)
  (TAttribute a1) <> (preview _TAttribute -> Just a2) = TAttribute (a1 <> a2)
  _               <> a2                               = a2

-- | 'TAttribute's are transformed directly, 'MAttribute's have their
--   local scale multiplied by the average scale of the transform.
--   Plain 'Attribute's are unaffected.
instance (Additive v, Traversable v, Floating n) => Transformable (Attribute v n) where
  transform _ (Attribute a)  = Attribute a
  transform t (MAttribute a) = MAttribute $ scaleLocal (avgScale t) a
  transform t (TAttribute a) = TAttribute $ transform t a

-- | Shows the kind of attribute and the type contained in the
--   attribute.
instance Show (Attribute v n) where
  showsPrec d attr = showParen (d > 10) $ case attr of
    Attribute a  -> showString "Attribute "  . showsPrec 11 (typeOf a)
    MAttribute a -> showString "MAttribute " . showsPrec 11 (mType a)
    TAttribute a -> showString "TAttribute " . showsPrec 11 (typeOf a)

-- | Unwrap an unknown 'Attribute' type, performing a dynamic (but
--   safe) check on the type of the result. If the required type
--   matches the type of the attribute, the attribute value is
--   returned wrapped in @Just@; if the types do not match, @Nothing@
--   is returned.
--
--   Measured attributes cannot be extrated from this function until
--   they have been unmeasured with 'unmeasureAttribute'. If you want a
--   measured attibute use the '_MAttribute' prism.
unwrapAttribute :: AttributeClass a => Attribute v n -> Maybe a
unwrapAttribute (Attribute a)  = cast a
unwrapAttribute (MAttribute _) = Nothing
unwrapAttribute (TAttribute a) = cast a
{-# INLINE unwrapAttribute #-}

-- | Prism onto an 'Attribute'.
_Attribute :: AttributeClass a => Prism' (Attribute v n) a
_Attribute = prism' Attribute $ \t -> case t of Attribute a -> cast a; _ -> Nothing
{-# INLINE _Attribute #-}

-- | Prism onto an 'MAttribute'.
_MAttribute :: (AttributeClass a, Typeable n) => Prism' (Attribute v n) (Measured n a)
_MAttribute = prism' MAttribute $ \t -> case t of MAttribute a -> cast a; _ -> Nothing
{-# INLINE _MAttribute #-}

-- | Prism onto a 'TAttribute'.
_TAttribute :: (V a ~ v, N a ~ n, AttributeClass a, Transformable a)
            => Prism' (Attribute v n) a
_TAttribute = prism' TAttribute $ \t -> case t of TAttribute a -> cast a; _ -> Nothing
{-# INLINE _TAttribute #-}

-- | Turn an 'MAttribute' into an 'Attribute' using the given 'global'
--   and 'normalized' scale.
unmeasureAttribute :: (Num n)
                   => n -> n -> Attribute v n -> Attribute v n
unmeasureAttribute g n (MAttribute m) = Attribute (fromMeasured g n m)
unmeasureAttribute _ _ a              = a

-- | Type of an attribute that is stored with a style. Measured
--   attributes return the type as if it where unmeasured.
attributeType :: Attribute v n -> TypeRep
attributeType (Attribute a)  = typeOf a
attributeType (MAttribute a) = mType a
attributeType (TAttribute a) = typeOf a

-- Note that we use type 'a' not 'Measured n a' so we don't have to rebuild
-- when unmeasuring the attributes.
mType :: forall n a. Typeable a => Measured n a -> TypeRep
mType _ = typeOf (undefined :: a)

-- naming convention: "Attribute" deals with the 'AttibuteType'
-- directly and "Attr" is for other things (like styles). Users should
-- rarely (if at all) deal with the 'Attibute' type directly.

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

-- instances -----------------------------------------------------------

type instance V (Style v n) = v
type instance N (Style v n) = n

instance Rewrapped (Style v n) (Style v' n')
instance Wrapped (Style v n) where
  type Unwrapped (Style v n) = HM.HashMap TypeRep (Attribute v n)
  _Wrapped' = iso (\(Style m) -> m) Style
  {-# INLINE _Wrapped' #-}

instance Each (Style v n) (Style v' n') (Attribute v n) (Attribute v' n') where
  each = _Wrapped . each
  {-# INLINE each #-}

type instance Index (Style v n)   = TypeRep
type instance IxValue (Style v n) = Attribute v n

instance Ixed (Style v n) where
  ix k = _Wrapped' . ix k
  {-# INLINE ix #-}

instance At (Style v n) where
  at k = _Wrapped' . at k
  {-# INLINE at #-}

-- | Combine a style by combining the attributes; if the two styles have
--   attributes of the same type they are combined according to their
--   semigroup structure.
instance Typeable n => Semigroup (Style v n) where
  Style s1 <> Style s2 = Style $ HM.unionWith (<>) s1 s2

-- | The empty style contains no attributes.
instance Typeable n => Monoid (Style v n) where
  mempty  = Style HM.empty
  mappend = (<>)

instance (Additive v, Traversable v, Floating n) => Transformable (Style v n) where
  transform t = over each (transform t)

-- | Styles have no action on other monoids.
instance A.Action (Style v n) m

-- | Show the attributes in the style.
instance Show (Style v n) where
  showsPrec d sty = showParen (d > 10) $
    showString "Style " . showsPrec d (sty ^.. each)

-- making styles -------------------------------------------------------

-- | Turn an attribute into a style. An easier way to make a style is to
--   use the monoid instance and apply library functions for applying
--   that attribute:
--
-- @
-- myStyle = mempty # fc blue :: Style V2 Double
-- @
attributeToStyle :: Attribute v n -> Style v n
attributeToStyle a = Style $ HM.singleton (attributeType a) a

-- extracting attributes -----------------------------------------------

-- | Extract an attribute from a style of a particular type.  If the
--   style contains an attribute of the requested type, it will be
--   returned wrapped in @Just@; otherwise, @Nothing@ is returned.
--
--   Trying to extract a measured attibute will fail. It either has to
--   be unmeasured with 'unmeasureAttrs' or use the 'atMAttr' lens.
getAttr :: forall a v n. AttributeClass a => Style v n -> Maybe a
getAttr (Style s) = HM.lookup ty s >>= unwrapAttribute
  where ty = typeOf (undefined :: a)
  -- unwrapAttribute can fail if someone tries to unwrap a measured
  -- attribute before it gets "unmeasured"

-- | Replace all 'MAttribute's with 'Attribute's using the 'global' and
--   'normalized' scales.
unmeasureAttrs :: (Num n) => n -> n -> Style v n -> Style v n
unmeasureAttrs g n = over each (unmeasureAttribute g n)

-- style lenses --------------------------------------------------------

mkAttrLens :: forall v n a. Typeable a
           => (a -> TypeRep)
           -> Prism' (Attribute v n) a
           -> Lens' (Style v n) (Maybe a)
mkAttrLens tyF p f sty =
  f (sty ^? ix ty . p) <&> \mAtt -> sty & at ty .~ (review p <$> mAtt)
  where ty = tyF (undefined :: a)
{-# INLINE mkAttrLens #-}

-- | Lens onto a plain attribute of a style.
atAttr :: AttributeClass a
       => Lens' (Style v n) (Maybe a)
atAttr = mkAttrLens typeOf _Attribute
{-# INLINE atAttr #-}

-- | Lens onto a measured attribute of a style.
atMAttr :: (AttributeClass a, Typeable n)
        => Lens' (Style v n) (Maybe (Measured n a))
atMAttr = mkAttrLens mType _MAttribute
{-# INLINE atMAttr #-}

-- | Lens onto a transformable attribute of a style.
atTAttr :: (V a ~ v, N a ~ n, AttributeClass a, Transformable a)
        => Lens' (Style v n) (Maybe a)
atTAttr = mkAttrLens typeOf _TAttribute
{-# INLINE atTAttr #-}

-- applying styles -----------------------------------------------------

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
--   diagram or a style). If the object already has an attribute of
--   the same type, the new attribute is combined on the left with the
--   existing attribute, according to their semigroup structure.
applyAttr :: (AttributeClass a, HasStyle d) => a -> d -> d
applyAttr = applyStyle . attributeToStyle . Attribute

-- | Apply a measured attribute to an instance of 'HasStyle' (such as a
--   diagram or a style). If the object already has an attribute of
--   the same type, the new attribute is combined on the left with the
--   existing attribute, according to their semigroup structure.
applyMAttr :: (AttributeClass a, N d ~ n, HasStyle d) => Measured n a -> d -> d
applyMAttr = applyStyle . attributeToStyle . MAttribute

-- | Apply a transformable attribute to an instance of 'HasStyle'
--   (such as a diagram or a style). If the object already has an
--   attribute of the same type, the new attribute is combined on the
--   left with the existing attribute, according to their semigroup
--   structure.
applyTAttr :: (AttributeClass a, Transformable a, V a ~ V d, N a ~ N d, HasStyle d) => a -> d -> d
applyTAttr = applyStyle . attributeToStyle . TAttribute

