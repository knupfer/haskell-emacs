{-# LANGUAGE NoImplicitPrelude #-}
module Text ( append
            , breakOn
            , breakOnAll
            , breakOnEnd
            , center
            , chunksOf
            , commonPrefixes
            , concat
            , cons
            , copy
            , count
            , drop
            , dropEnd
            , group
            , head
            , index
            , init
            , inits
            , intercalate
            , intersperse
            , isInfixOf
            , isPrefixOf
            , isSuffixOf
            , justifyLeft
            , justifyRight
            , last
            , length
            , lines
            , maximum
            , minimum
            , null
            , pack
            , replace
            , replicate
            , reverse
            , singleton
            , snoc
            , splitAt
            , splitOn
            , strip
            , stripEnd
            , stripPrefix
            , stripStart
            , stripSuffix
            , tail
            , tails
            , take
            , takeEnd
            , toCaseFold
            , toLower
            , toTitle
            , toUpper
            , transpose
            , uncons
            , unlines
            , unpack
            , unwords
            , words
            , zip) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude   (Bool, Char, Int, Maybe, String)

append :: Text -> Text -> Text
append = T.append

breakOn :: Text -> Text -> (Text, Text)
breakOn = T.breakOn

breakOnAll :: Text -> Text -> [(Text, Text)]
breakOnAll = T.breakOnAll

breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd = T.breakOnEnd

center :: Int -> Char -> Text -> Text
center = T.center

chunksOf :: Int -> Text -> [Text]
chunksOf = T.chunksOf

commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes = T.commonPrefixes

concat :: [Text] -> Text
concat = T.concat

cons :: Char -> Text -> Text
cons = T.cons

copy :: Text -> Text
copy = T.copy

count :: Text -> Text -> Int
count = T.count

drop :: Int -> Text -> Text
drop = T.drop

dropEnd :: Int -> Text -> Text
dropEnd = T.dropEnd

group :: Text -> [Text]
group = T.group

head :: Text -> Char
head = T.head

index :: Text -> Int -> Char
index = T.index

init :: Text -> Text
init = T.init

inits :: Text -> [Text]
inits = T.inits

intercalate :: Text -> [Text] -> Text
intercalate = T.intercalate

intersperse :: Char -> Text -> Text
intersperse = T.intersperse

isInfixOf :: Text -> Text -> Bool
isInfixOf = T.isInfixOf

isPrefixOf :: Text -> Text -> Bool
isPrefixOf = T.isPrefixOf

isSuffixOf :: Text -> Text -> Bool
isSuffixOf = T.isSuffixOf

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft = T.justifyLeft

justifyRight :: Int -> Char -> Text -> Text
justifyRight = T.justifyRight

last :: Text -> Char
last = T.last

length :: Text -> Int
length = T.length

lines :: Text -> [Text]
lines = T.lines

maximum :: Text -> Char
maximum = T.maximum

minimum :: Text -> Char
minimum = T.minimum

null :: Text -> Bool
null = T.null

pack :: String -> Text
pack = T.pack

replace :: Text -> Text -> Text -> Text
replace = T.replace

replicate :: Int -> Text -> Text
replicate = T.replicate

reverse :: Text -> Text
reverse = T.reverse

singleton :: Char -> Text
singleton = T.singleton

snoc :: Text -> Char -> Text
snoc = T.snoc

splitAt :: Int -> Text -> (Text, Text)
splitAt = T.splitAt

splitOn :: Text -> Text -> [Text]
splitOn = T.splitOn

strip :: Text -> Text
strip = T.strip

stripEnd :: Text -> Text
stripEnd = T.stripEnd

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix = T.stripPrefix

stripStart :: Text -> Text
stripStart = T.stripStart

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix = T.stripSuffix

tail :: Text -> Text
tail = T.tail

tails :: Text -> [Text]
tails = T.tails

take :: Int -> Text -> Text
take = T.take

takeEnd :: Int -> Text -> Text
takeEnd = T.takeEnd

toCaseFold :: Text -> Text
toCaseFold = T.toCaseFold

toLower :: Text -> Text
toLower = T.toLower

toTitle :: Text -> Text
toTitle = T.toTitle

toUpper :: Text -> Text
toUpper = T.toUpper

transpose :: [Text] -> [Text]
transpose = T.transpose

uncons :: Text -> Maybe (Char, Text)
uncons = T.uncons

unlines :: [Text] -> Text
unlines = T.unlines

unpack :: Text -> String
unpack = T.unpack

unwords :: [Text] -> Text
unwords = T.unwords

words :: Text -> [Text]
words = T.words

zip :: Text -> Text -> [(Char, Char)]
zip = T.zip
