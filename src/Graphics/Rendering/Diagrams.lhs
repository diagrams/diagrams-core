> {-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, GADTs #-}

XXX comment me

> module Graphics.Rendering.Diagrams where
>
> import Graphics.Rendering.Diagrams.Transform
> import Graphics.Rendering.Diagrams.Expressions
>
> import Data.VectorSpace
> import Data.Basis
> import Data.MemoTrie
>
> import Control.Monad (mapM_)
>
> import qualified Data.Map as M

| Abstract diagrams are rendered to particular formats by /backends/.
  Each backend must be an instance of the 'Backend' class, and comes
  with an associated vector space and rendering environment.

> class (HasBasis (BSpace b), HasTrie (Basis (BSpace b))) => Backend b where
>   type BSpace b :: *           -- ^ The vector space associated with this backend
>   type Render b :: * -> *      -- ^ The rendering environment used by this backend
>   type Result b :: *           -- ^ The result of the rendering operation
>   data Option b :: *           -- ^ The rendering options for this backend
>   renderDia     :: b -> [Option b] -> Diagram b -> Result b
>                                -- ^ Render a diagram using this backend
> 

| The 'Renderable' type class connects backends to primitives which
  they know how to render.

> class (Backend b, Transformable t) => Renderable t b where
>   render :: b -> t -> Render b ()  
>   -- ^ Given a token representing the backend and a transformable
>   --   object, render it in the appropriate rendering context.

| A value of type @Prim b@ is an opaque (existentially quantified)
  primitive which backend @b@ knows how to render.

> data Prim b where
>   Prim :: (BSpace b ~ TSpace t, Renderable t b) => t -> Prim b
> 
> renderPrim :: b -> Prim b -> Render b ()
> renderPrim b (Prim t) = render b t
> 
> instance Backend b => Transformable (Prim b) where
>   type TSpace (Prim b) = BSpace b
>   transform v (Prim p) = Prim (transform v p)
> 
> instance Backend b => Renderable (Prim b) b where
>   render b (Prim p) = render b p


> type Bounds v = v -> Scalar v
>
> data Diagram b = Diagram { prims  :: [Prim b]
>                          , bounds :: Bounds (BSpace b)
>                          , names  :: NameSet (BSpace b)
>                          }

> rebase :: ( Backend b, v ~ BSpace b
>           , InnerSpace v, HasBasis v, HasTrie (Basis v)
>           , AdditiveGroup (Scalar v), Fractional (Scalar v))
>        => LExpr v -> Diagram b -> Diagram b
> rebase e d = Diagram { prims  = map (translate (negateV u))
>                                     (prims d)
>                      , bounds = rebaseBounds u (bounds d)
>                      , names  = M.map (^-^ u) (names d)
>                      }
>   where u = evalLExpr e (names d)
> 
> rebaseBounds :: (InnerSpace v, AdditiveGroup (Scalar v), Fractional (Scalar v))
>              => v -> Bounds v -> Bounds v
> rebaseBounds u f v = f v ^-^ ((u ^/ (v <.> v)) <.> v)
> 
> instance ( Backend b
>          , HasTrie (Basis (BSpace b))
>          , HasBasis (BSpace b)) =>
>          Transformable (Diagram b) where
>   type TSpace (Diagram b) = BSpace b
>   transform t (Diagram ps bs ns) = Diagram (map (transform t) ps)
>                                            (\v -> undefined)    -- XXX
>                                            (M.map (aapply t) ns)
> 
> instance (Backend b, Monad (Render b)) => Renderable (Diagram b) b where
>   render b (Diagram ps _ _) = mapM_ (render b) ps
> 
> atop :: Ord (Scalar (BSpace b)) => Diagram b -> Diagram b -> Diagram b
> atop (Diagram ps1 bs1 ns1) (Diagram ps2 bs2 ns2) =
>   Diagram (ps1 ++ ps2)
>           (\v -> max (bs1 v) (bs2 v))  -- XXX make this nicer?
>           (M.union ns1 ns2)
> 
> beside :: ( Backend b
>           , v ~ BSpace b
>           , HasBasis v
>           , HasTrie (Basis v)
>           , InnerSpace v
>           , AdditiveGroup (Scalar v)
>           , Fractional (Scalar v)
>           , Ord (Scalar v))
>        => v -> Diagram b -> Diagram b -> Diagram b
> beside v d1 d2 = rebase (Const (bounds d1 v *^ v)) d1
>           `atop` rebase (Const (bounds d2 (negateV v) *^ negateV v)) d2