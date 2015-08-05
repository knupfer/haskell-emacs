{-# LANGUAGE OverloadedStrings #-}

module HaskellEmacsTest (nothing,
             unicode,
             unicodeText,
             constantText,
             concatString,
             concatText,
             constantString,
             constantTrue,
             constantFalse,
             notBool,
             allTrue,
             anyTrue,
             nextNum,
             summation,
             constant,
             squareRoot,
             bothTrue,
             multiply,
             takeSome,
             switch,
             concatFst,
             longAnswer,
             nthFib) where

import qualified Data.Text as T

-- String

nothing :: String -> String
nothing = const ""

unicode :: String
unicode = "ˈiːmæks\
          \إيماكس\
          \ایمکس\
          \이맥스\
          \И́макс"

unicodeText :: T.Text
unicodeText = "ˈiːmæks\
              \إيماكس\
              \ایمکس\
              \이맥스\
              \И́макс"

constantText :: T.Text
constantText = T.pack "test"

concatString :: [String] -> String
concatString = concat

concatText :: [T.Text] -> T.Text
concatText = T.concat

constantString :: T.Text
constantString = "test"

-- Bool

constantTrue :: Bool
constantTrue = True

constantFalse :: Bool
constantFalse = False

notBool :: Bool -> Bool
notBool = not

allTrue :: [Bool] -> Bool
allTrue = and

anyTrue :: [Bool] -> Bool
anyTrue = or

-- Num

nextNum :: Int -> Int
nextNum = (+1)

summation :: [Int] -> Int
summation = sum

constant :: Double
constant = 10.5

squareRoot :: Double -> Double
squareRoot = sqrt

-- Multiple arguments

bothTrue :: Bool -> Bool -> Bool
bothTrue = (&&)

multiply :: Int -> Int -> Int
multiply a b = a * b

takeSome :: Int -> String -> String
takeSome = take

-- Tuple

switch :: (String, Int) -> (Int, String)
switch (s,i) = (i,s)

concatFst :: [(String,Int)] -> String
concatFst = concatMap fst

-- benchmarks

longAnswer :: Int -> String
longAnswer n = replicate (2^n) 'a'

nthFib :: Int -> Int
nthFib = (!!) fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
