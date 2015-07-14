module Char ( chr
            , digitToInt
            , generalCategory
            , intToDigit
            , isAlpha
            , isAlphaNum
            , isAscii
            , isAsciiLower
            , isAsciiUpper
            , isControl
            , isDigit
            , isHexDigit
            , isLatin1
            , isLetter
            , isLower
            , isMark
            , isNumber
            , isOctDigit
            , isPrint
            , isPunctuation
            , isSeparator
            , isSpace
            , isSymbol
            , isUpper
            , ord
            , toLower
            , toTitle
            , toUpper ) where

import           Data.AttoLisp
import qualified Data.Char     as C
import qualified Data.Text     as T

chr :: Int -> Char
chr = C.chr

digitToInt :: Char -> Int
digitToInt = C.digitToInt

generalCategory :: Char -> Lisp
generalCategory c = List [Symbol . T.pack . show $ C.generalCategory c]

intToDigit :: Int -> Char
intToDigit = C.intToDigit

isAlpha :: Char -> Bool
isAlpha = C.isAlpha

isAlphaNum :: Char -> Bool
isAlphaNum = C.isAlphaNum

isAscii :: Char -> Bool
isAscii = C.isAscii

isAsciiLower :: Char -> Bool
isAsciiLower = C.isAsciiLower

isAsciiUpper :: Char -> Bool
isAsciiUpper = C.isAsciiUpper

isControl :: Char -> Bool
isControl = C.isControl

isDigit :: Char -> Bool
isDigit = C.isDigit

isHexDigit :: Char -> Bool
isHexDigit = C.isHexDigit

isLatin1 :: Char -> Bool
isLatin1 = C.isLatin1

isLetter :: Char -> Bool
isLetter = C.isLetter

isLower :: Char -> Bool
isLower = C.isLower

isMark :: Char -> Bool
isMark = C.isMark

isNumber :: Char -> Bool
isNumber = C.isNumber

isOctDigit :: Char -> Bool
isOctDigit = C.isOctDigit

isPrint :: Char -> Bool
isPrint = C.isPrint

isPunctuation :: Char -> Bool
isPunctuation = C.isPunctuation

isSeparator :: Char -> Bool
isSeparator = C.isSeparator

isSpace :: Char -> Bool
isSpace = C.isSpace

isSymbol :: Char -> Bool
isSymbol = C.isSymbol

isUpper :: Char -> Bool
isUpper = C.isUpper

ord :: Char -> Int
ord = C.ord

toLower :: Char -> Char
toLower = C.toLower

toTitle :: Char -> Char
toTitle = C.toTitle

toUpper :: Char -> Char
toUpper = C.toUpper
