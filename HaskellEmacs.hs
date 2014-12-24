{-# LANGUAGE OverloadedStrings #-}
---- <<import>> ----
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Parallel
import qualified Control.Parallel.Strategies as PS
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString  as A (parseOnly)
import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.ByteString.UTF8        as B (fromString, toString)
import qualified Data.Map                    as M
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           System.IO                   (hFlush, stdout)
import Data.Maybe

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
main = do printer <- newEmptyMVar
          forkIO . forever $ takeMVar printer >>= B.putStrLn >> hFlush stdout
          forever $
            do (f,n,ls) <- extract <$> T.getLine
               xs <- replicateM ls T.getLine
               let r = run f n xs
               forkIO $ (r `PS.using` PS.rdeepseq) `pseq` putMVar printer r
       where extract = (\(x:y:z:_) -> (x, y, read $ T.unpack z)) . T.words

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: T.Text -> T.Text -> [T.Text] -> B.ByteString
run f n xs = B.concat [ "("
                      , B.pack . show $ (length . B.toString $ B.toStrict result) - 4
                      , " "
                      , B.pack $ T.unpack n
                      , result
                      ]
        where result = fromJust (M.lookup f dispatcher) $ T.unlines xs
