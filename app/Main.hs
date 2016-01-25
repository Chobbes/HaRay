{- Copyright (C) 2016 Calvin Beck
   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import HaRay.Objects.Spheres
import HaRay.Trace
import HaRay.Vectors
import HaRay.Rays
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter


main :: IO ()
main = print . run $ A.map (flip trace sphere) (pixelRays 800 400 (constant (0,0,0)))
  where sphere = constant $ Sphere (0,0,10) 1


pixelRays :: Int -> Int -> Exp Vec3 -> Acc (Array DIM2 Ray)
pixelRays width height ori = generate (constant (Z :. width :. height)) indexToRay
  where
    indexToRay :: Exp DIM2 -> Exp Ray
    indexToRay ix = makeRay ori dir
      where (Z :. y :. x) = unlift ix
            a = A.fromIntegral x
            b = A.fromIntegral y
            dir = makeVec a b 1


makeVec :: Exp Double -> Exp Double -> Exp Double -> Exp (Double, Double, Double)
makeVec x y z = lift (x, y, z)


makeRay :: Exp (Double, Double, Double) -> Exp (Double, Double, Double) -> Exp Ray
makeRay o d = lift (Ray o d)
