{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Base ( abs
            , acos
            , acosh
            , and
            , asin
            , asinh
            , asTypeOf
            , atan
            , atan2
            , atanh
            , ceiling
            , compare
            , concat
            , const
            , cos
            , cosh
            , cycle
            , decodeFloat
            , div
            , divMod
            , drop
            , elem
            , encodeFloat
            , enumFrom
            , enumFromThen
            , enumFromThenTo
            , enumFromTo
            , error
            , even
            , exp
            , exponent
            , floatDigits
            , floatRadix
            , floatRange
            , floor
            , fst
            , gcd
            , head
            , id
            , init
            , isDenormalized
            , isIEEE
            , isInfinite
            , isNaN
            , isNegativeZero
            , last
            , length
            , lcm
            , lines
            , log
            , logBase
            , lookup
            , mappend
            , max
            , maximum
            , mconcat
            , mempty
            , min
            , minimum
            , mod
            , negate
            , not
            , notElem
            , null
            , odd
            , or
            , otherwise
            , pi
            , product
            , properFraction
            , quot
            , quotRem
            , recip
            , rem
            , repeat
            , replicate
            , reverse
            , round
            , scaleFloat
            , seq
            , show
            , significand
            , signum
            , sin
            , sinh
            , snd
            , splitAt
            , subtract
            , sqrt
            , sum
            , tail
            , take
            , tan
            , tanh
            , truncate
            , undefined
            , unlines
            , unwords
            , unzip
            , unzip3
            , words
            , zip
            , zip3) where

import           Data.AttoLisp
import           Data.Text     (Text)
import           Prelude       (Bool, Double, Int, Integer, Maybe, String)
import qualified Prelude       as P

abs :: Double -> Double
abs = P.abs

acos :: Double -> Double
acos = P.acos

acosh :: Double -> Double
acosh = P.acosh

and :: [Bool] -> Bool
and = P.and

asin :: Double -> Double
asin = P.asin

asinh :: Double -> Double
asinh = P.asinh

asTypeOf :: Lisp -> Lisp -> Lisp
asTypeOf a@(Symbol _ ) (Symbol _ ) = a
asTypeOf a@(String _ ) (String _ ) = a
asTypeOf a@(Number _ ) (Number _ ) = a
asTypeOf a@(List   _ ) (List   _ ) = a
asTypeOf a@(DotList{}) (DotList{}) = a
asTypeOf _ _ = List [Symbol "error", String "Arguments don't have the same type."]

atan :: Double -> Double
atan = P.atan

atan2 :: Double -> Double -> Double
atan2 = P.atan2

atanh :: Double -> Double
atanh = P.atanh

ceiling :: Double -> Integer
ceiling = P.ceiling

compare :: Lisp -> Lisp -> Lisp
compare a b = case P.compare a b of
                   P.LT -> Symbol "LT"
                   P.EQ -> Symbol "EQ"
                   P.GT -> Symbol "GT"

concat :: [[Lisp]] -> [Lisp]
concat = P.concat

const :: Lisp -> Lisp -> Lisp
const a _ = a

cos :: Double -> Double
cos = P.cos

cosh :: Double -> Double
cosh = P.cosh

cycle :: [Lisp] -> [Lisp]
cycle = P.cycle

decodeFloat :: Double -> (Integer, Int)
decodeFloat = P.decodeFloat

div :: Integer -> Integer -> Integer
div = P.div

divMod :: Integer -> Integer -> (Integer, Integer)
divMod = P.divMod

drop :: Int -> [Lisp] -> [Lisp]
drop = P.drop

elem :: Lisp -> [Lisp] -> Bool
elem = P.elem

encodeFloat :: Integer -> Int -> Double
encodeFloat = P.encodeFloat

enumFrom :: Double -> [Double]
enumFrom = P.enumFrom

enumFromThen :: Double -> Double -> [Double]
enumFromThen = P.enumFromThen

enumFromThenTo :: Double -> Double -> Double -> [Double]
enumFromThenTo = P.enumFromThenTo

enumFromTo :: Double -> Double -> [Double]
enumFromTo = P.enumFromTo

error :: Text -> Lisp
error s = List [Symbol "error", String s]

even :: Integer -> Bool
even = P.even

exp :: Double -> Double
exp = P.exp

exponent :: Double -> Int
exponent = P.exponent

floatDigits :: Double -> Int
floatDigits = P.floatDigits

floatRadix :: Double -> Integer
floatRadix = P.floatRadix

floatRange :: Double -> (Int, Int)
floatRange = P.floatRange

floor :: Double -> Integer
floor = P.floor

fst :: (Lisp, Lisp) -> Lisp
fst = P.fst

gcd :: Integer -> Integer -> Integer
gcd = P.gcd

head :: [Lisp] -> Lisp
head (x:_) = x
head _ = List [Symbol "error", String "Head on empty list."]

id :: Lisp -> Lisp
id = P.id

init :: [Lisp] -> [Lisp]
init l@(_:_) = P.init l
init _ = [Symbol "error", String "Init on empty list."]

isDenormalized :: Double -> Bool
isDenormalized = P.isDenormalized

isIEEE :: Double -> Bool
isIEEE = P.isIEEE

isInfinite :: Double -> Bool
isInfinite = P.isInfinite

isNaN :: Double -> Bool
isNaN = P.isNaN

isNegativeZero :: Double -> Bool
isNegativeZero = P.isNegativeZero

last :: [Lisp] -> Lisp
last l@(_:_) = P.last l
last _ = List [Symbol "error", String "Last on empty list."]

length :: [Lisp] -> Int
length = P.length

lcm :: Integer -> Integer -> Integer
lcm = P.lcm

lines :: String -> [String]
lines = P.lines

log :: Double -> Double
log = P.log

logBase :: Double -> Double -> Double
logBase = P.logBase

lookup :: Lisp -> [(Lisp, Lisp)] -> Maybe Lisp
lookup = P.lookup

mappend :: [Lisp] -> [Lisp] -> [Lisp]
mappend = (P.++)

max :: Lisp -> Lisp -> Lisp
max = P.max

maximum :: [Lisp] -> Lisp
maximum = P.maximum

mconcat :: [[Lisp]] -> [Lisp]
mconcat = P.concat

mempty :: [Lisp] -> [Lisp]
mempty _ = []

min :: Lisp -> Lisp -> Lisp
min = P.min

minimum :: [Lisp] -> Lisp
minimum = P.minimum

mod :: Integer -> Integer -> Integer
mod = P.mod

negate :: Double -> Double
negate = P.negate

not :: Bool -> Bool
not = P.not

notElem :: Lisp -> [Lisp] -> Bool
notElem = P.notElem

null :: [Lisp] -> Bool
null = P.null

odd :: Integer -> Bool
odd = P.odd

or :: [Bool] -> Bool
or = P.or

otherwise :: Bool
otherwise = P.True

pi :: Double
pi = P.pi

product :: [Double] -> Double
product = P.product

properFraction :: Double -> (Integer, Double)
properFraction = P.properFraction

quot :: Integer -> Integer -> Integer
quot = P.quot

quotRem :: Integer -> Integer -> (Integer, Integer)
quotRem = P.quotRem

recip :: Double -> Double
recip = P.recip

rem :: Integer -> Integer -> Integer
rem = P.rem

repeat :: Lisp -> [Lisp]
repeat = P.repeat

replicate :: Int -> Lisp -> [Lisp]
replicate = P.replicate

reverse :: [Lisp] -> [Lisp]
reverse = P.reverse

round :: Double -> Integer
round = P.round

scaleFloat :: Int -> Double -> Double
scaleFloat = P.scaleFloat

seq :: Lisp -> Lisp -> Lisp
seq = P.seq

show :: Lisp -> String
show = P.show

significand :: Double -> Double
significand = P.significand

signum :: Double -> Double
signum = P.signum

sin :: Double -> Double
sin = P.sin

sinh :: Double -> Double
sinh = P.sinh

snd :: (Lisp,Lisp) -> Lisp
snd = P.snd

splitAt :: Int -> [Lisp] -> ([Lisp], [Lisp])
splitAt = P.splitAt

subtract :: Double -> Double -> Double
subtract = P.subtract

sqrt :: Double -> Double
sqrt = P.sqrt

sum :: [Double] -> Double
sum = P.sum

tail :: [Lisp] -> [Lisp]
tail l@(_:_) = tail l
tail _ = [Symbol "error", String "Tail on empty list."]

take :: Int -> [Lisp] -> [Lisp]
take = P.take

tan :: Double -> Double
tan = P.tan

tanh :: Double -> Double
tanh = P.tanh

truncate :: Double -> Integer
truncate = P.truncate

undefined :: Lisp
undefined = List [Symbol "error", String "undefined"]

unlines :: [String] -> String
unlines = P.unlines

unwords :: [String] -> String
unwords = P.unwords

unzip :: [(Lisp, Lisp)] -> ([Lisp], [Lisp])
unzip = P.unzip

unzip3 :: [(Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp])
unzip3 = P.unzip3

words :: String -> [String]
words = P.words

zip :: [Lisp] -> [Lisp] -> [(Lisp, Lisp)]
zip = P.zip

zip3 :: [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp)]
zip3 = P.zip3
