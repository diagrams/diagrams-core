> {-# LANGUAGE TypeFamilies, FlexibleContexts #-}

XXX some comments here

> module Graphics.Rendering.Diagrams.Backends where
>
> import Data.Basis
> import Data.MemoTrie
>
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
>   runRender :: [RenderOption] -> Render b () -> IO ()
>                                -- ^ Run a computation in the rendering environment
