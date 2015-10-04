{-# LANGUAGE OverloadedStrings #-}

module HaskellEmacsTest where

import           Control.Monad
import           Data.Char
import qualified Data.Text        as T
import           External.NBody
import           Foreign.Emacs
import           System.IO.Unsafe

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

-- Char

nextChar :: Char -> Char
nextChar = chr . succ . ord

-- Lisp

symbolReverse :: Lisp -> Lisp
symbolReverse (Symbol s) = Symbol $ T.reverse s
symbolReverse x          = x

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

multiply :: Double -> Double -> Double
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

doNBody :: Integer -> Double
doNBody = unsafePerformIO . nbody

emptyEmacsMonad :: Emacs ()
emptyEmacsMonad = return ()

emacsMonad :: Int -> Emacs Int
emacsMonad n = do a <- replicateM n $ eval [Symbol "identity", Number 1]
                  return $ sum a

emacsMonad_ :: Int -> Emacs Int
emacsMonad_ n = do replicateM_ n $ eval_ [Symbol "identity", Number 1]
                   return n
