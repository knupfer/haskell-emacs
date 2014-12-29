{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
---- <<import>> ----
import           Control.Applicative         ((<$>))
import           Control.Concurrent
import           Control.Monad               (forever, replicateM)
import           Control.Parallel.Strategies (rdeepseq, using)
import           Data.AttoLisp
import           Data.Attoparsec.ByteString  (parseOnly)
import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding
import qualified Data.Text.IO                as T
import           System.IO                   (hFlush, stdout)
import qualified Text.Show.Text              as T (show)

class Arity f where
  arity :: f -> Int

instance Arity x where
  arity _ = 0

instance Arity f => Arity ((->) a f) where
  arity f = 1 + arity (f undefined)

-- | Watch for commands and dispatch them in a seperate fork.
main :: IO ()
main = if not $ null arityList
          then B.putStrLn . encode $ toDispatcher arityList
          else do
            printer <- newEmptyMVar
            forkIO . forever $ takeMVar printer >>= T.putStrLn >> hFlush stdout
            forever $
              do (f,n,line) <- extract <$> T.getLine
                 result     <- run f n <$> replicateM line T.getLine
                 forkIO $ (result `using` rdeepseq) `seq` putMVar printer result

toDispatcher :: [(Text, Int)] -> (Text, [Text])
toDispatcher xs = ( T.intercalate "," $ map fun xs
                  , map (\(_,x) -> T.unwords $ "(": args x ++ [")"]) xs)
  where wrap t ts = T.concat $ "(\"":t: "\",transform ": (ts :: [Text]) ++ [")"]
        fun (t,n) | n == 0 = wrap t [ "((const :: a -> Int -> a) ", t, ")" ]
                  | n == 1 = wrap t [t]
                  | otherwise = wrap t [ "(\\(", T.intercalate "," $ args n
                                       , ") -> " , T.unwords $ t:args n, ")"]
        args n = ["x" <> T.show x | x <- [1 .. n ]]

arityList :: [(Text,Int)]
arityList =
  [
  ---- <<arity>> ----
  ]

-- | Map of available functions which get transformed to produce and
-- receive strings.
dispatcher :: M.Map Text (Text -> Text)
dispatcher = M.fromList $
  [ ("arityFormat", transform arityFormat)
  , ("allExports", transform allExports)] ++ [
  ---- <<export>> ----
  ]

allExports :: [Text] -> (Text, [Text])
allExports xs = if null l
                    then ("",[""])
                    else (T.concat $ map head l, concatMap tail l)
  where l = filter (not . null) $ map exportsGet xs

exportsGet :: Text -> [Text]
exportsGet t
  | length list < 2 = []
  | otherwise = (\(x:xs) -> imports x :map ((x <> ".") <>) xs) list
  where list = filter (not . T.null) . takeWhile (/= "where")
               . drop 1 . dropWhile (/= "module") $ T.split (`elem` "\n ,()\t") t
        imports x = "import qualified " <>x<> "\n"

arityFormat :: [Text] -> Text
arityFormat = T.intercalate "," . map (\x -> T.concat ["(\"", x, "\",arity ", x, ")"])

-- | Transform a curried function to a function which receives and
-- returns a string in lisp syntax.
transform :: (FromLisp a, ToLisp b) => (a -> b) -> Text -> Text
transform f = either (T.pack . (++) " nil)") toText . parseOnly lisp . encodeUtf8
  where toText = failure . fmap (decodeUtf8 . B.toStrict . encode . f) . fromLisp

-- | Retrieves the contents of the result and annotates whether it was
-- a success.
failure :: Result Text -> Text
failure (Success s) = " yes)" <> s
failure (Error s)   = T.pack $ " nil)" ++ s

extract :: Text -> (Text, Text, Int)
extract = (\(x:y:z:_) -> (x, y, read $ T.unpack z)) . T.words

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: Text -> Text -> [Text] -> Text
run f resultId xs = T.concat ["(", msgLength, " ", resultId, result]
  where result    = fromJust (M.lookup f dispatcher) $ T.unlines xs
        msgLength = T.show . T.length $ T.dropWhile (/= ')') result
