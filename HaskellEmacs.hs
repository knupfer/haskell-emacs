{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
---- <<import>> ----
import           Control.Applicative             ((<$>))
import           Control.Concurrent
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8      as B
import qualified Data.Map                        as M
import           Data.Maybe                      (fromJust)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.Encoding
import qualified Data.Text.IO                    as T
import           System.IO                       (hFlush, stdout)
import qualified Text.Show.Text                  as T (show)

class Arity f where
  arity :: f -> Int

instance Arity x where
  arity _ = 0

instance Arity f => Arity ((->) a f) where
  arity f = 1 + arity (f undefined)

-- | Watch for commands and dispatch them in a seperate fork.
main :: IO ()
main = do lock <- newMVar ()
          mapM_ (\(fun,l) -> forkIO $ fun l `seq` modifyMVar_ lock
                        . const $ T.putStr (fun l) >> hFlush stdout)
                =<< fullParse <$> B.getContents

fullParse :: B.ByteString -> [(Lisp -> Text, Lisp)]
fullParse c = map snd . tail $ iterate nextParse (c,(const "",nil))

nextParse :: (B.ByteString, t) -> (B.ByteString, (Lisp -> Text, Lisp))
nextParse (c, _) = case parseInput c of A.Done a b -> (a,b)
                                        A.Fail a _ _ -> (a, (const "",nil))

parseInput :: B.ByteString -> A.Result (Lisp -> Text, Lisp)
parseInput = A.parse $ do
  a <- A.takeTill $ A.inClass " "
  _ <- A.string " "
  b <- A.takeTill $ A.inClass " "
  _ <- A.string " "
  c <- lisp
  _ <- A.string "\n"
  return (run (decodeUtf8 a) (decodeUtf8 b), c)

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
dispatcher :: M.Map Text (Lisp -> Text)
dispatcher = M.fromList $
  [ ("arityFormat", transform arityFormat)
  , ("allExports", transform allExports)
  , ("arityList", transform . (const :: a -> Int -> a) $ toDispatcher arityList)
  , ("formatCode", transform $ uncurry formatCode)
  ] ++ [
  ---- <<export>> ----
  ]

formatCode :: (Text,Text,Text) -> Text -> Text
formatCode (imports, exports, arities) = inject "arity"  arities
                                       . inject "export" exports
                                       . inject "import" imports
  where inject s = T.replace (T.concat ["---- <<",s,">> ----"])

allExports :: [Text] -> (Text, [Text])
allExports xs = if null l
                   then ("",[""])
                   else (T.concat $ map head l, concatMap tail l)
  where l = filter (not . null) $ map exportsGet xs

exportsGet :: Text -> [Text]
exportsGet t
  | length list < 2 = []
  | otherwise       = (\(x:xs) -> imports x : map ((x <> ".") <>) xs) list
  where list = filter (not . T.null) . takeWhile (/= "where")
               . drop 1 . dropWhile (/= "module") $ T.split (`elem` "\n ,()\t") t
        imports x = "import qualified " <>x<> "\n"

arityFormat :: [Text] -> Text
arityFormat = T.intercalate ","
              . map (\x -> T.concat ["(\"", x, "\",arity ", x, ")"])

-- | Transform a curried function to a function which receives and
-- returns a string in lisp syntax.
transform :: (FromLisp a, ToLisp b) => (a -> b) -> Lisp -> Text
transform f = failure . fmap (decodeUtf8 . B.toStrict . encode . f) . fromLisp

-- | Retrieves the contents of the result and annotates whether it was
-- a success.
failure :: Result Text -> Text
failure (Success s) = ")" <> s
failure (Error s)   = T.pack $ " t)" ++ s

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: Text -> Text -> Lisp -> Text
run f resultId xs = T.concat ["(", msgLength, " ", resultId, result]
  where result    = fromJust (M.lookup f dispatcher) xs
        msgLength = T.show . T.length . T.drop 1 $ T.dropWhile (/= ')') result
