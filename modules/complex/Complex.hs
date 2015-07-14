{-# LANGUAGE NoImplicitPrelude #-}
module Complex ( cis
               , conjugate
               , imagPart
               , magnitude
               , mkPolar
               , phase
               , polar
               , realPart ) where

import qualified Data.Complex as C
import Prelude hiding (Real)

type Real = Double
type Imaginary = Double
type Magnitude = Double
type Phase = Double

complexToTuple :: C.Complex Double -> (Real, Imaginary)
complexToTuple c = (C.realPart c, C.imagPart c)

tupleToComplex :: (Real, Imaginary) -> C.Complex Double
tupleToComplex = uncurry (C.:+)

-------------------------------------------------

cis :: Phase -> (Real, Imaginary)
cis = mkPolar 1

conjugate :: (Real, Imaginary) -> (Real, Imaginary)
conjugate t = complexToTuple . C.conjugate $ tupleToComplex t

imagPart :: (Real, Imaginary) -> Imaginary
imagPart = snd

magnitude :: (Real, Imaginary) -> Magnitude
magnitude = C.magnitude . tupleToComplex

mkPolar :: Magnitude -> Phase -> (Real, Imaginary)
mkPolar a b = complexToTuple $ C.mkPolar a b

phase :: (Real, Imaginary) -> Phase
phase = C.phase . tupleToComplex

polar :: (Real, Imaginary) -> (Magnitude, Phase)
polar = C.polar . tupleToComplex

realPart :: (Real, Imaginary) -> Real
realPart = fst
