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
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import qualified  Graphics.Gloss.Accelerate.Data.Color.RGBA as AC
import Graphics.Gloss.Accelerate.Data.Picture


main :: IO ()
main = display (InWindow "HaRay" (width, height) (0,0))
               red
               (bitmapOfArray (run . colourScene $ renderScene width height ori sphere) True)
  where sphere = constant $ Sphere (0,0,10) 9
        width = 800
        height = 400
        ori = constant (0,0,0)

colourScene :: Acc (Array DIM2 Double) -> Acc (Array DIM2 Word32)
colourScene = A.map (\x -> x >* 0 ? (AC.packRGBA AC.blue, AC.packRGBA AC.black))

renderScene :: Int -> Int -> Exp Vec3 -> Exp Sphere -> Acc (Array DIM2 Double)
renderScene width height ori sphere = A.map (flip trace sphere) (pixelRays width height ori)

pixelRays :: Int -> Int -> Exp Vec3 -> Acc (Array DIM2 Ray)
pixelRays width height ori = generate (constant (Z :. height :. width)) indexToRay
  where
    midX = A.fromIntegral (constant width) / 2
    midY = A.fromIntegral (constant height) / 2
    indexToRay :: Exp DIM2 -> Exp Ray
    indexToRay ix = makeRay ori dir
      where (Z :. y :. x) = unlift ix
            a = A.fromIntegral x - midX
            b = A.fromIntegral y - midY
            dir = normalize $ makeVec a b 20


makeVec :: Exp Double -> Exp Double -> Exp Double -> Exp (Double, Double, Double)
makeVec x y z = lift (x, y, z)


makeRay :: Exp (Double, Double, Double) -> Exp (Double, Double, Double) -> Exp Ray
makeRay o d = lift (Ray o d)
