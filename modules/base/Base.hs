{-# LANGUAGE NoImplicitPrelude #-}
module Base ( even
            , odd
            , rem
            , or
            , exponent
            , properFraction
            , and
            , cos
            , cosh
            , exp
            , floor
            , product
            , recip
            , round
            , div
            , divMod
            , quot
            , quotRem
            , abs
            , acos
            , acosh
            , asin
            , asinh
            , atan
            , atan2
            , atanh
            , ceiling
            , gcd
            , lcm
            , lines
            , log
            , logBase
            , mod
            , negate
            , not
            , significand
            , signum
            , sin
            , sinh
            , sqrt
            , sum
            , tan
            , tanh
            , truncate
            , unlines
            , unwords
            , words) where

import qualified Prelude as P

abs :: P.Double -> P.Double
abs = P.abs

acos :: P.Double -> P.Double
acos = P.acos

acosh :: P.Double -> P.Double
acosh = P.acosh

and :: [P.Bool] -> P.Bool
and = P.and

asin :: P.Double -> P.Double
asin = P.asin

asinh :: P.Double -> P.Double
asinh = P.asinh

atan :: P.Double -> P.Double
atan = P.atan

atan2 :: P.Double -> P.Double -> P.Double
atan2 = P.atan2

atanh :: P.Double -> P.Double
atanh = P.atanh

ceiling :: P.Double -> P.Integer
ceiling = P.ceiling

cos :: P.Double -> P.Double
cos = P.cos

cosh :: P.Double -> P.Double
cosh = P.cosh

div :: P.Integer -> P.Integer -> P.Integer
div = P.div

divMod :: P.Integer -> P.Integer -> (P.Integer, P.Integer)
divMod = P.divMod

even :: P.Integer -> P.Bool
even = P.even

exp :: P.Double -> P.Double
exp = P.exp

exponent :: P.Double -> P.Int
exponent = P.exponent

floor :: P.Double -> P.Integer
floor = P.floor

gcd :: P.Integer -> P.Integer -> P.Integer
gcd = P.gcd

lcm :: P.Integer -> P.Integer -> P.Integer
lcm = P.lcm

lines :: P.String -> [P.String]
lines = P.lines

log :: P.Double -> P.Double
log = P.log

logBase :: P.Double -> P.Double -> P.Double
logBase = P.logBase

mod :: P.Integer -> P.Integer -> P.Integer
mod = P.mod

negate :: P.Double -> P.Double
negate = P.negate

not :: P.Bool -> P.Bool
not = P.not

odd :: P.Integer -> P.Bool
odd = P.odd

or :: [P.Bool] -> P.Bool
or = P.or

product :: [P.Double] -> P.Double
product = P.product

properFraction :: P.Double -> (P.Integer, P.Double)
properFraction = P.properFraction

quot :: P.Integer -> P.Integer -> P.Integer
quot = P.quot

quotRem :: P.Integer -> P.Integer -> (P.Integer, P.Integer)
quotRem = P.quotRem

recip :: P.Double -> P.Double
recip = P.recip

rem :: P.Integer -> P.Integer -> P.Integer
rem = P.rem

round :: P.Double -> P.Integer
round = P.round

significand :: P.Double -> P.Double
significand = P.significand

signum :: P.Double -> P.Double
signum = P.signum

sin :: P.Double -> P.Double
sin = P.sin

sinh :: P.Double -> P.Double
sinh = P.sinh

sqrt :: P.Double -> P.Double
sqrt = P.sqrt

sum :: [P.Integer] -> P.Integer
sum = P.sum

tan :: P.Double -> P.Double
tan = P.tan

tanh :: P.Double -> P.Double
tanh = P.tanh

truncate :: P.Double -> P.Integer
truncate = P.truncate

unlines :: [P.String] -> P.String
unlines = P.unlines

unwords :: [P.String] -> P.String
unwords = P.unwords

words :: P.String -> [P.String]
words = P.words
