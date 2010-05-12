{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}
module Test where

import Diagrams

import Data.Monoid

type Point = (Double, Double)

data Printer2D = Printer2D

instance Backend Printer2D where
  type BSpace Printer2D = (Double, Double)
  type Render Printer2D = IO
  runRender _ = id

data Ellipse = Ellipse (Affine (Double,Double))

instance Transformable Ellipse where
  type TSpace Ellipse = (Double, Double)
  transform a (Ellipse c) = Ellipse (a `mappend` c)

instance Renderable Ellipse Printer2D where
  render _ (Ellipse c) = print (aapply c (1,0)) >> print (aapply c (0,1))