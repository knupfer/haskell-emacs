{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module List ( and
            , concat
            , cycle
            , delete
            , drop
            , elem
            , elemIndex
            , elemIndices
            , head
            , init
            , inits
            , insert
            , intercalate
            , intersect
            , intersperse
            , isInfixOf
            , isPrefixOf
            , isSubsequenceOf
            , isSuffixOf
            , last
            , length
            , lines
            , lookup
            , maximum
            , minimum
            , notElem
            , nub
            , null
            , or
            , permutations
            , product
            , repeat
            , replicate
            , reverse
            , sort
            , splitAt
            , stripPrefix
            , subsequences
            , sum
            , tail
            , tails
            , take
            , transpose
            , uncons
            , union
            , unlines
            , unwords
            , unzip
            , unzip3
            , unzip4
            , unzip5
            , unzip6
            , unzip7
            , words
            , zip
            , zip3
            , zip4
            , zip5
            , zip6
            , zip7 ) where

import           Data.AttoLisp
import qualified Data.List     as L
import           Prelude       (Bool, Double, Int, Maybe, String)

and :: [Bool] -> Bool
and = L.and

concat :: [[Lisp]] -> [Lisp]
concat = L.concat

cycle :: [Lisp] -> [Lisp]
cycle = L.cycle

delete :: Lisp -> [Lisp] -> [Lisp]
delete = L.delete

drop :: Int -> [Lisp] -> [Lisp]
drop = L.drop

elem :: Lisp -> [Lisp] -> Bool
elem = L.elem

elemIndex :: Lisp -> [Lisp] -> Maybe Int
elemIndex = L.elemIndex

elemIndices :: Lisp -> [Lisp] -> [Int]
elemIndices = L.elemIndices

head :: [Lisp] -> Lisp
head (x:_) = x
head _ = List [Symbol "error", String "Head on empty list."]

init :: [Lisp] -> [Lisp]
init l@(_:_) = L.init l
init _ = [Symbol "error", String "Init on empty list."]

inits :: [Lisp] -> [[Lisp]]
inits = L.inits

insert :: Lisp -> [Lisp] -> [Lisp]
insert = L.insert

intercalate :: [Lisp] -> [[Lisp]] -> [Lisp]
intercalate = L.intercalate

intersect :: [Lisp] -> [Lisp] -> [Lisp]
intersect = L.intersect

intersperse :: Lisp -> [Lisp] -> [Lisp]
intersperse = L.intersperse

isInfixOf :: [Lisp] -> [Lisp] -> Bool
isInfixOf = L.isInfixOf

isPrefixOf :: [Lisp] -> [Lisp] -> Bool
isPrefixOf = L.isPrefixOf

isSubsequenceOf :: [Lisp] -> [Lisp] -> Bool
isSubsequenceOf = L.isSubsequenceOf

isSuffixOf :: [Lisp] -> [Lisp] -> Bool
isSuffixOf = L.isSuffixOf

last :: [Lisp] -> Lisp
last l@(_:_) = L.last l
last _ = List [Symbol "error", String "Last on empty list."]

length :: [Lisp] -> Int
length = L.length

lines :: String -> [String]
lines = L.lines

lookup :: Lisp -> [(Lisp, Lisp)] -> Maybe Lisp
lookup = L.lookup

maximum :: [Lisp] -> Lisp
maximum = L.maximum

minimum :: [Lisp] -> Lisp
minimum = L.minimum

notElem :: Lisp -> [Lisp] -> Bool
notElem = L.notElem

nub :: [Lisp] -> [Lisp]
nub = L.nub

null :: [Lisp] -> Bool
null = L.null

or :: [Bool] -> Bool
or = L.or

permutations :: [Lisp] -> [[Lisp]]
permutations = L.permutations

product :: [Double] -> Double
product = L.product

repeat :: Lisp -> [Lisp]
repeat = L.repeat

replicate :: Int -> Lisp -> [Lisp]
replicate = L.replicate

reverse :: [Lisp] -> [Lisp]
reverse = L.reverse

sort :: [Lisp] -> [Lisp]
sort = L.sort

splitAt :: Int -> [Lisp] -> ([Lisp], [Lisp])
splitAt = L.splitAt

stripPrefix :: [Lisp] -> [Lisp] -> Maybe [Lisp]
stripPrefix = L.stripPrefix

subsequences :: [Lisp] -> [[Lisp]]
subsequences = L.subsequences

sum :: [Double] -> Double
sum = L.sum

tail :: [Lisp] -> [Lisp]
tail l@(_:_) = L.tail l
tail _ = [Symbol "error", String "Tail on empty list."]

tails :: [Lisp] -> [[Lisp]]
tails = L.tails

take :: Int -> [Lisp] -> [Lisp]
take = L.take

transpose :: [[Lisp]] -> [[Lisp]]
transpose = L.transpose

uncons :: [Lisp] -> Maybe (Lisp, [Lisp])
uncons = L.uncons

union :: [Lisp] -> [Lisp] -> [Lisp]
union = L.union

unlines :: [String] -> String
unlines = L.unlines

unwords :: [String] -> String
unwords = L.unwords

unzip :: [(Lisp, Lisp)] -> ([Lisp], [Lisp])
unzip = L.unzip

unzip3 :: [(Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp])
unzip3 = L.unzip3

unzip4 :: [(Lisp, Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp], [Lisp])
unzip4 = L.unzip4

unzip5 :: [(Lisp, Lisp, Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp], [Lisp], [Lisp])
unzip5 = L.unzip5

unzip6 :: [(Lisp, Lisp, Lisp, Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp], [Lisp], [Lisp], [Lisp])
unzip6 = L.unzip6

unzip7 :: [(Lisp , Lisp, Lisp, Lisp, Lisp, Lisp, Lisp)] -> ([Lisp], [Lisp], [Lisp], [Lisp], [Lisp], [Lisp], [Lisp])
unzip7 = L.unzip7

words :: String -> [String]
words = L.words

zip :: [Lisp] -> [Lisp] -> [(Lisp, Lisp)]
zip = L.zip

zip3 :: [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp)]
zip3 = L.zip3

zip4 :: [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp, Lisp)]
zip4 = L.zip4

zip5 :: [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp, Lisp, Lisp)]
zip5 = L.zip5

zip6 :: [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp, Lisp, Lisp, Lisp)]
zip6 = L.zip6

zip7 :: [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [Lisp] -> [(Lisp, Lisp, Lisp, Lisp, Lisp, Lisp, Lisp)]
zip7 = L.zip7



