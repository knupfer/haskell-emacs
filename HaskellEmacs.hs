{-# LANGUAGE OverloadedStrings #-}
---- <<import>> ----
import           Control.Applicative
import           Control.Monad
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString as A (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8       as B (fromString,toString)
import qualified Data.Map                   as M
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           System.IO                  (hFlush, stdout)

-- | Map of available functions which get transformed to produce and
-- receive strings.
dispatcher :: M.Map T.Text (T.Text -> B.ByteString)
dispatcher = M.fromList
 [
 ---- <<export>> ----
 ]

-- | Transform a curried function to a function which receives and
-- returns a string in lisp syntax.
transform :: (FromLisp a, ToLisp b) => (a -> b) -> T.Text -> B.ByteString
transform f = either (B.pack . (++) " nil)")
                      (failure . fmap (encode . f) . fromLisp)
                      . A.parseOnly lisp . B.fromString . T.unpack

-- | Retrieves the contents of the result and annotates whether it was
-- a success.
failure :: Result B.ByteString -> B.ByteString
failure (Success s) = " yes)" <> s
failure (Error s)   = B.pack $ " nil)" ++ s

-- | Lookup functions given in stdin in the dispatcher.
main :: IO ()
main = do
    (f,n,ls) <- (\(x:y:z:_) -> (x,y,read $ T.unpack z)) . T.words <$> T.getLine
    case M.lookup f dispatcher of
      Just function -> replicateM ls T.getLine >>=  B.putStr . run function n
                      >> hFlush stdout >> main
      Nothing       -> main

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: (T.Text -> B.ByteString) -> T.Text -> [T.Text] -> B.ByteString
run f n ls = B.concat [ "("
                      , B.pack . show $ (length . B.toString $ B.toStrict $ result) - 5
                      , " "
                      , B.pack $ T.unpack n
                      , result
                      ]
        where result = f $ T.unlines ls
