> {-# LANGUAGE TypeFamilies, FlexibleContexts #-}

This module defines the interface for diagram rendering backends.
Each backend has an associated vector space, and uses an associated
rendering context.  Adding a new rendering backend is as simple as
instantiating the 'Backend' class, and then writing a 'Renderable'
instance (see "Graphics.Rendering.Diagrams.Renderable") for each
primitive that your backend knows how to render.

> module Graphics.Rendering.Diagrams.Backends where
>
> import Data.Basis
> import Data.MemoTrie
>

| Options to control rendering.  NOTE, more options need to be added
  here.  Not every backend needs to recognize every option; unrecognized
  options should simply be ignored.

> data RenderOption =
>     OutputFile FilePath
>   -- XXX other things here, like size etc.
>   | Other String String
> 

| Abstract diagrams are rendered to particular formats by /backends/.
  Each backend must be an instance of the 'Backend' class, and comes
  with an associated vector space and rendering environment.

> class (HasBasis (BSpace b), HasTrie (Basis (BSpace b))) => Backend b where
>   type BSpace b :: *           -- ^ The vector space associated with this backend
>   type Render b :: * -> *      -- ^ The rendering environment used by this backend
>   runRender :: b -> [RenderOption] -> Render b () -> IO ()
>                                -- ^ Run a computation in the rendering environment
