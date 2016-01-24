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

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module HaRay.Vectors where

import Data.Array.Accelerate as A


type Vec a = (a, a, a)
type Vec3 = Vec Double


vecZipWith :: (Elt a, Elt b, Elt c) => (Exp a -> Exp b -> Exp c) -> Exp (Vec a) -> Exp (Vec b) -> Exp (Vec c)
vecZipWith f u v = lift (f ux vx, f uy vy, f uz vz)
  where (ux, uy, uz) = unlift u
        (vx, vy, vz) = unlift v


vecMap :: (Elt a, Elt b) => (Exp a -> Exp b) -> Exp (Vec a) -> Exp (Vec b)
vecMap f v = lift (f x, f y, f z)
  where (x, y, z) = unlift v


magnitude :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp a
magnitude = sqrt . vecSquare


dot :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp a
dot u v = x + y + z
  where
    (x, y, z) = unlift prod
    prod = u .*. v


vecSquare :: (Elt a, IsNum a) => Exp (Vec a) -> Exp a
vecSquare v = v `dot` v


normalize :: (Elt a, IsFloating a) => Exp (Vec a) -> Exp (Vec a)
normalize v = v .* (1 / magnitude v)


-- Vector and vector
(.+.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(.+.) = vecZipWith (+)

(.-.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(.-.) = vecZipWith (-)

(.*.) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp (Vec a) -> Exp (Vec a)
(.*.) = vecZipWith (*)


-- Vector and scalar
(.*) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp a -> Exp (Vec a)
v .* s = vecMap (*s) v

(.-) :: (Elt a, IsNum a) => Exp (Vec a) -> Exp a -> Exp (Vec a)
v .- s = vecMap (subtract s) v


-- Scalar and vector
(*.) :: (Elt a, IsNum a) => Exp a -> Exp (Vec a) -> Exp (Vec a)
s *. v = vecMap (s*) v
