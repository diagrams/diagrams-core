> {-# LANGUAGE GADTs, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

XXX comment me

> module Graphics.Rendering.Diagrams.Renderable where
>
> import Graphics.Rendering.Diagrams.Backends
> import Graphics.Rendering.Diagrams.Transform

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