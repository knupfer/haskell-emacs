module Bits ( bit
            , bitDefault
            , bitSizeMaybe
            , clearBit
            , complement
            , complementBit
            , countLeadingZeros
            , countTrailingZeros
            , finiteBitSize
            , isSigned
            , popCount
            , popCountDefault
            , rotate
            , rotateL
            , rotateR
            , setBit
            , shift
            , shiftL
            , shiftR
            , testBit
            , testBitDefault
            , toIntegralSized
            , unsafeShiftL
            , unsafeShiftR
            , xor
            , zeroBits ) where

import qualified Data.Bits as B

bit :: Int -> Integer
bit = B.bit

bitDefault :: Int -> Integer
bitDefault = B.bitDefault

bitSizeMaybe :: Integer -> Maybe Int
bitSizeMaybe = B.bitSizeMaybe

clearBit :: Integer -> Int -> Integer
clearBit = B.clearBit

complement :: Integer -> Integer
complement = B.complement

complementBit :: Integer -> Int -> Integer
complementBit = B.complementBit

countLeadingZeros :: Int -> Int
countLeadingZeros = B.countLeadingZeros

countTrailingZeros :: Int -> Int
countTrailingZeros = B.countTrailingZeros

finiteBitSize :: Int -> Int
finiteBitSize = B.finiteBitSize

isSigned :: Integer -> Bool
isSigned = B.isSigned

popCount :: Integer -> Int
popCount = B.popCount

popCountDefault :: Integer -> Int
popCountDefault = B.popCountDefault

rotate :: Integer -> Int -> Integer
rotate = B.rotate

rotateL :: Integer -> Int -> Integer
rotateL = B.rotateL

rotateR :: Integer -> Int -> Integer
rotateR = B.rotateR

setBit :: Integer -> Int -> Integer
setBit = B.setBit

shift :: Integer -> Int -> Integer
shift = B.shift

shiftL :: Integer -> Int -> Integer
shiftL = B.shiftL

shiftR :: Integer -> Int -> Integer
shiftR = B.shiftR

testBit :: Integer -> Int -> Bool
testBit = B.testBit

testBitDefault :: Integer -> Int -> Bool
testBitDefault = B.testBitDefault

toIntegralSized :: Integer -> Maybe Integer
toIntegralSized = B.toIntegralSized

unsafeShiftL :: Integer -> Int -> Integer
unsafeShiftL = B.unsafeShiftL

unsafeShiftR :: Integer -> Int -> Integer
unsafeShiftR = B.unsafeShiftR

xor :: Integer -> Integer -> Integer
xor = B.xor

zeroBits :: Integer
zeroBits = 0
