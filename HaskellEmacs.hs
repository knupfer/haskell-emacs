{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{--<<import>>--}
import           Control.Applicative              ((<$>))
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad                    (forever)
import           Control.Parallel.Strategies
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy  as A
import qualified Data.ByteString.Lazy.Char8       as B hiding (length)
import qualified Data.ByteString.Lazy.UTF8        as B (length)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           System.IO                        (hFlush, stdout)

class Arity f where
  arity :: f -> Int

instance Arity x where
  arity _ = 0

instance Arity f => Arity ((->) a f) where
  arity f = 1 + arity (f undefined)

-- | Watch for commands and dispatch them in a seperate fork.
main :: IO ()
main = do printer <- newChan
          forkIO . forever $ readChan printer >>= B.putStr >> hFlush stdout
          -- the lambda is necessary for a dependency on calculated tuples
          mapM_ (\(fun,l) -> forkIO $ writeChan printer $! fun l)
                =<< fullParse <$> B.getContents

traverseLisp :: Lisp -> Result Lisp
traverseLisp l = case l of
  List (Symbol x:xs) -> sym x xs
  List xs            -> list xs
  _                  -> Success l
  where eval     = sequence . parMap rdeepseq traverseLisp
        list     = fmap List . eval
        sym x xs | x `elem` ["t", "nil"] = List . (:) (Symbol x) <$> eval xs
                 | length xs /= 1 = run x =<< List <$> eval xs
                 | otherwise = run x =<< traverseLisp (head xs)

fullParse :: B.ByteString -> [(Lisp -> B.ByteString, Lisp)]
fullParse c = case parseInput c of A.Done a b -> b : fullParse a
                                   A.Fail {}  -> []

parseInput :: B.ByteString -> A.Result (Lisp -> B.ByteString, Lisp)
parseInput = A.parse $ do
  i <- A.option 0 AC.decimal
  l <- lisp
  return (resultToText i . traverseLisp, l)

getDocumentation :: [T.Text]  -> T.Text -> [T.Text]
getDocumentation funs code =
  map ( \f -> T.unlines . (++) (filter (T.isPrefixOf (f <> " ::")) ls ++ [""])
      . reverse
      . map (T.dropWhile (`elem` ("- |" :: String)))
      . takeWhile (T.isPrefixOf "-- ")
      . reverse
      $ takeWhile (not . T.isPrefixOf (f <> " ")) ls
      ) funs
  where ls = T.lines code

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: Text -> Lisp -> Result Lisp
run = fromJust . flip M.lookup dispatcher

resultToText :: Int -> Result Lisp -> B.ByteString
resultToText i l = case l of
       Success s -> f [ ] $ encode s
       Error s   -> f [1] $ B.pack s
   where f err t = encode ([B.length t, i] ++ err) <> t

-- | Map of available functions which get transformed to produce and
-- receive strings.
dispatcher :: M.Map Text (Lisp -> Result Lisp)
dispatcher = M.fromList $
  [ ("arityFormat", transform arityFormat)
  , ("allExports",  transform allExports)
  , ("arityList",   transform . (const :: a -> Lisp -> a) $ toDispatcher arityList)
  , ("formatCode",  transform $ uncurry formatCode)
  , ("getDocumentation", transform $ uncurry getDocumentation)
  ] ++ [{--<<export>>--}]

-- | Transform a curried function to a function which receives and
-- returns a string in lisp syntax.
transform :: (FromLisp a, ToLisp b) => (a -> b) -> Lisp -> Result Lisp
transform f = fmap (toLisp . f) . fromLisp

toDispatcher :: [(Text, Int)] -> (Text, [Text])
toDispatcher = T.intercalate "," . map fun &&& map (pars . args . snd)
  where args n    = T.unwords [T.pack $ 'x' : show x | x <- [1..n]]
        pars x    = "(" <> x <> ")"
        fun (t,n) = pars . wrap . T.unwords $ case n of
          0 -> ["(const :: a -> Lisp -> a)", t]
          1 -> [t]
          _ -> ["\\", pars . T.replace " " "," $ args n, "->", t, args n]
          where wrap ts = "\"" <> t <> "\",transform" <> pars ts

-- Helperfunctions for bootstrapping.

arityList :: [(Text, Int)]
arityList = [{--<<arity>>--}]

formatCode :: (Text, Text, Text) -> Text -> Text
formatCode (imports, exports, arities) = inject "arity"  arities
                                       . inject "export" exports
                                       . inject "import" imports
  where inject s = T.replace ("{--<<" <> s <> ">>--}")

allExports :: [Text] -> (Text, [Text])
allExports = g . filter (not . null) . map exportsGet
  where g = T.unlines . map head &&& concatMap tail

exportsGet :: Text -> [Text]
exportsGet t
  | length list < 2 = []
  | otherwise       = (\(x:xs) -> imports x : map ((x <> ".") <>) xs) list
  where list = filter (not . T.null) . takeWhile (/= "where")
               . drop 1 . dropWhile (/= "module") $ T.split (`elem` ("\n ,()\t" :: String))
               . T.unlines . map (fst . T.breakOn "--") $ T.lines t
        imports = (<>) "import qualified "

arityFormat :: [Text] -> Text
arityFormat = T.intercalate ","
              . map (\x -> "(\"" <> x <> "\",arity " <> x <> ")")
