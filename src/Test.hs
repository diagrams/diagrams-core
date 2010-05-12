{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
module Test where

import Diagrams

import Data.Monoid
import qualified Data.Map as M

type Point2 = (Double, Double)

data Printer2D = Printer2D

instance Backend Printer2D where
  type BSpace Printer2D = Point2
  type Render Printer2D = IO
  runRender _ = id

data Ellipse = Ellipse (Affine Point2)

instance Transformable Ellipse where
  type TSpace Ellipse = Point2
  transform a (Ellipse c) = Ellipse (a `mappend` c)

instance Renderable Ellipse Printer2D where
  render _ (Ellipse c) = print (aapply c (1,0)) >> print (aapply c (0,1))

data Box = Box Point2 Point2 Point2 Point2

instance Transformable Box where
  type TSpace Box = Point2
  transform a (Box v1 v2 v3 v4) = Box (aapply a v1)
                                      (aapply a v2)
                                      (aapply a v3)
                                      (aapply a v4)

instance Renderable Box Printer2D where
  render _ (Box v1 v2 v3 v4) = mapM_ print [v1, v2, v3, v4]

box :: (BSpace b ~ Point2, Renderable Box b) => Diagram b
box = Diagram [Prim (Box (0,0) (1,0) (1,1) (0,1))]
              boxBounds
              (M.fromList [ (nm "LL", (0,0))
                          , (nm "LR", (1,0))
                          , (nm "UR", (1,1))
                          , (nm "UL", (0,1)) ])
  where boxBounds (x,y) = let d = x*x + y*y
                          in  maximum [x/d, y/d, (x+y)/d, 0]