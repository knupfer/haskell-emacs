{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where
{--<<import>>--}
import           Control.Applicative              ((<$>))
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad                    (forever)
import           Control.Parallel.Strategies
import           Data.AttoLisp                    
import           Data.Maybe                       (fromJust,fromMaybe, catMaybes)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text, unpack, pack)
import           Language.Haskell.Exts.Parser     (parseModule, ParseResult(..))
import           Language.Haskell.Exts.Syntax     (Module(..), ExportSpec(..), QName(..), Name, Decl(FunBind, PatBind), Match(Match), Pat(..))
import           System.IO                        (hFlush, stdout)
import qualified Data.Attoparsec.ByteString.Char8 as AC 
import qualified Data.Attoparsec.ByteString.Lazy  as A
import qualified Data.ByteString.Lazy.Char8       as B hiding (length)
import qualified Data.ByteString.Lazy.UTF8        as B (length)
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Language.Haskell.Exts.Syntax     as S (Name(Ident, Symbol)) 

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

-- | Recursively evaluate a lisp in parallel, using functions defined
-- by the user (see documentation of the emacs function `haskell-emacs-init').
traverseLisp :: Lisp -> Result Lisp
traverseLisp l = case l of
  List (Symbol x:xs) -> sym (T.filter (/='\\') x) xs
  List xs            -> list xs
  Symbol "nil"       -> Success nil
  _                  -> Success l
  where eval     = sequence . parMap rdeepseq traverseLisp
        list     = fmap List . eval
        sym x xs | x `notElem` M.keys dispatcher = List . (:) (Symbol x) <$> eval xs
                 | length xs /= 1 = run x =<< List <$> eval xs
                 | otherwise = run x =<< traverseLisp (head xs)

-- | Takes an stream of instructions and returns a parsed list of
-- functions.
fullParse :: B.ByteString -> [(Lisp -> B.ByteString, Lisp)]
fullParse c = case parseInput c of A.Done a b -> b : fullParse a
                                   A.Fail {}  -> []

-- | Parse an instruction and stamp the number of the instruction into
-- the resulting function.
parseInput :: B.ByteString -> A.Result (Lisp -> B.ByteString, Lisp)
parseInput = A.parse $ do
  i <- A.option 0 AC.decimal
  l <- lisp
  return (resultToText i . traverseLisp, l)

-- | Scrape the documentation of haskell functions to serve it in emacs.
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

-- | Takes a function (described in a Text) and feeds it a lisp.
run :: Text -> Lisp -> Result Lisp
run = fromJust . flip M.lookup dispatcher

resultToText :: Int -> Result Lisp -> B.ByteString
resultToText i l = case l of
       Success s -> f [ ] $ encode s
       Error s   -> f [1] $ B.pack s
   where f err t = encode ([B.length t, i] ++ err) <> t

-- | Map of available functions which get transformed to work on lisp.
dispatcher :: M.Map Text (Lisp -> Result Lisp)
dispatcher = M.fromList $
  [ ("arityFormat", transform arityFormat . normalize)
  , ("allExports",  transform allExports)
  , ("arityList",   transform . (const :: a -> Lisp -> a) $ toDispatcher arityList)
  , ("formatCode",  transform $ uncurry formatCode)
  , ("getDocumentation", transform $ uncurry getDocumentation)
  ] ++
  [ {--<<export>>--} ]

-- | Transform a curried function to a function which receives and
-- returns lisp forms.
transform :: (FromLisp a, ToLisp b) => (a -> b) -> Lisp -> Result Lisp
transform f = fmap (toLisp . f) . fromLisp

-- | Prevent bad input for the bootstrap.
normalize :: Lisp -> Lisp
normalize l@(List _)      = l
normalize l@(DotList _ _) = l
normalize a               = List [a]

-- | Takes tuples of function names and their arities and returns
-- haskell source code which gets spliced back into a module.
toDispatcher :: [(Text, Int)] -> (Text, [Text])
toDispatcher fs = T.intercalate "," . map fun &&& map (pars . args . snd) $ fs
  where args n    = T.unwords [T.pack $ 'x' : show x | x <- [1..n]]
        pars x    = "(" <> x <> ")"
        fun (t,1) = wrap t t
        fun (t,n) = wrap t . T.unwords . ("$" :) $ case n of
          0 -> [pars "const :: a -> Lisp -> a", t]
          2 -> ["uncurry", t]
          _ -> ["\\", pars . T.replace " " "," $ args n, "->", t, args n]
        wrap t ts = pars $ T.justifyLeft padding ' ' ("\"" <> t <> "\",")
                    <> "transform " <> ts
        padding = maximum [T.length f | (f,_) <- fs] + 4

-- | List of functions and their arities (filled by emacs).
arityList :: [(Text, Int)]
arityList =
  [ {--<<arity>>--} ]

-- | Splice user functions into the haskell module.
formatCode :: (Text, Text, Text) -> Text -> Text
formatCode (imports, exports, arities) = inject "arity"  (pretty arities)
                                       . inject "export" (pretty exports)
                                       . inject "import" imports
  where inject s = T.replace ("{--<<" <> s <> ">>--}")
        pretty = T.replace "),(" ")\n  , ("

-- | Import statement of all modules and all their functions.
allExports :: [Text] -> (Text, [Text])
allExports = g . filter (not . null) . map exportsGet
  where g = T.unlines . map head &&& concatMap tail

-- | List of haskell functions which get querried for their arity.
arityFormat :: [Text] -> Text
arityFormat ts = T.intercalate ","
                 . map (\x -> "(" <> T.justifyLeft padding ' ' ("\"" <> x <> "\",")
                              <> "arity " <> x <> ")")
                 $ ts
  where padding = maximum [T.length t | t <- ts] + 4

-- | Retrieve list of exported functions in a haskell module.
exportsGet :: Text -> [Text]
exportsGet moduleContent =
  case parseModule (unpack moduleContent) of
    ParseOk (Module _ _ _ _ Nothing _ decls)    -> exportsFromModuleDecls decls
    ParseOk (Module _ _ _ _ (Just exspecs) _ _) -> exportsFromHeader exspecs
    ParseFailed loc msg                         -> error msg

exportsFromModuleDecls :: [Decl] -> [Text]
exportsFromModuleDecls = catMaybes . fmap functionDeclarationNames

functionDeclarationNames :: Decl -> Maybe Text
functionDeclarationNames (FunBind [])                         = Nothing
functionDeclarationNames (FunBind (Match _ nm _ _ _ _ : _)) = Just (fromName nm)
functionDeclarationNames (PatBind _ (PVar nm) _ _)            = Just (fromName nm)
functionDeclarationNames _                                    = Nothing

-- Extract the unqalified function names from an ExportSpec
exportsFromHeader :: [ExportSpec] -> [Text]
exportsFromHeader =
  catMaybes . fmap (fmap fromName  . exportFunction)

fromName :: Name -> Text
fromName (S.Symbol str) = pack str
fromName (S.Ident str)  = pack str

exportFunction :: ExportSpec -> Maybe Name
exportFunction (EVar _ qname)      = unQalifiedName qname
exportFunction (EModuleContents _) = Nothing
exportFunction _                   = Nothing

unQalifiedName :: QName -> Maybe Name
unQalifiedName (Qual _ name) = Just name
unQalifiedName (UnQual name) = Just name
unQalifiedName _             = Nothing
