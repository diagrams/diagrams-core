{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleContexts
           , TypeSynonymInstances
           , FlexibleInstances #-}
module Test where

import Diagrams

import Data.Monoid
import qualified Data.Map as M

type Point2 = (Double, Double)

instance Transformable Point2 where
  type TSpace Point2 = Point2
  transform = aapply

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
  transform a (Box v1 v2 v3 v4) = Box (transform a v1)
                                      (transform a v2)
                                      (transform a v3)
                                      (transform a v4)

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

instance Transformable v => Transformable (Segment v) where
  type TSpace (Segment v) = TSpace v
  transform a (Bezier v1 v2 v3) = Bezier (transform a v1)
                                         (transform a v2)
                                         (transform a v3)

instance Renderable (Segment Point2) Printer2D where
  render _ (Bezier c1 c2 x2) = putStrLn "bezier"
                               >> mapM_ (putStrLn . ("  "++) . show) [c1, c2, x2]

bez2 :: (BSpace b ~ Point2, Renderable (Segment Point2) b)
     => Point2 -> Point2 -> Point2 -> Diagram b
bez2 c1 c2 x2 = Diagram [Prim b]
                        (segmentBounds b)
                        (M.fromList [ (nm "X1", (0,0))
                                    , (nm "C1", c1)
                                    , (nm "C2", c2)
                                    , (nm "X2", x2) ])
  where b = Bezier c1 c2 x2