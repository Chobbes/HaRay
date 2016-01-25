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

module HaRay.Rays where

import HaRay.Vectors
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar ( Elt(..), EltRepr, EltRepr' )
import Data.Typeable


type Ray = RayA Double


data RayA a = Ray (Vec a) -- ^ Origin
                  (Vec a) -- ^ Direction
  deriving (Eq, Show, Typeable)


type instance EltRepr (RayA a) = EltRepr (Vec a, Vec a)
type instance EltRepr' (RayA a) = EltRepr' (Vec a, Vec a)


instance Elt a => Elt (RayA a) where
  eltType (_ :: RayA a) = eltType (undefined :: (Vec a, Vec a))
  fromElt (Ray o d) = fromElt (o, d)
  toElt packed = let (o, d) = toElt packed in Ray o d

  eltType' (_ :: RayA a) = eltType' (undefined :: (Vec a, Vec a))
  fromElt' (Ray o d) = fromElt' (o, d)
  toElt' packed = let (o, d) = toElt' packed in Ray o d


instance IsTuple (RayA a) where
  type TupleRepr (RayA a) = TupleRepr (Vec a, Vec a)
  fromTuple (Ray o d) = (((), o), d)
  toTuple (((), o), d) = Ray o d


instance (Lift Exp a, Elt (Plain a)) => Lift Exp (RayA a) where
  type Plain (RayA a) = RayA (Plain a)
  lift (Ray o d) = Exp . Tuple $ NilTup `SnocTup` lift o `SnocTup` lift d


rayOri :: Exp Ray -> Exp Vec3
rayOri ray = Exp $ SuccTupIdx ZeroTupIdx `Prj` ray


rayDir :: Exp Ray -> Exp Vec3
rayDir ray = Exp $ ZeroTupIdx `Prj` ray


