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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaRay.Objects.Spheres where

import HaRay.Trace
import HaRay.Rays
import HaRay.Vectors
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar ( Elt(..), EltRepr, EltRepr' )
import Data.Typeable


data Sphere = Sphere Vec3 -- ^ Centre
                     Double -- ^ Radius
  deriving (Eq, Show, Typeable)


type instance EltRepr Sphere = EltRepr (Vec3, Double)
type instance EltRepr' Sphere = EltRepr' (Vec3, Double)


instance Elt Sphere where
  eltType (_ :: Sphere) = eltType (undefined :: (Vec3, Double))
  fromElt (Sphere c r) = fromElt (c, r)
  toElt packed = let (c, r) = toElt packed in Sphere c r

  eltType' (_ :: Sphere) = eltType' (undefined :: (Vec3, Double))
  fromElt' (Sphere c r) = fromElt' (c, r)
  toElt' packed = let (c, r) = toElt' packed in Sphere c r


instance IsTuple Sphere where
  type TupleRepr Sphere = TupleRepr (Vec3, Double)
  fromTuple (Sphere c r) = (((), c), r)
  toTuple (((), c), r) = Sphere c r


sphereCentre :: Exp Sphere -> Exp Vec3
sphereCentre sphere = Exp $ SuccTupIdx ZeroTupIdx `Prj` sphere


sphereRadius :: Exp Sphere -> Exp Double
sphereRadius sphere = Exp $ ZeroTupIdx `Prj` sphere


instance Trace Sphere where
--  trace :: Exp Ray -> Exp a -> Exp Double
  trace ray sphere = (radicand >* 0) ? (numerator / denomenator, -1)
    where
      centreToRay = c .-. o
      radicand = 4 * (vecSquare (v .*. centreToRay) - (vecSquare v) * (vecSquare centreToRay) + r^2)
      numerator = 2 * (v `dot` centreToRay) - sqrt radicand
      denomenator = 2 * (vecSquare v)

      -- Unpacking sphere
      c = sphereCentre sphere
      r = sphereRadius sphere

      -- Unpacking ray
      o = rayOri ray
      v = rayDir ray
