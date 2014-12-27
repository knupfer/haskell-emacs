{-# LANGUAGE OverloadedStrings #-}
---- <<import>> ----
import           Control.Applicative         ((<$>))
import           Control.Concurrent
import           Control.Monad               (forever, replicateM)
import           Control.Parallel.Strategies (rdeepseq, using)
import           Data.AttoLisp
import           Data.Attoparsec.ByteString  (parseOnly)
import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.ByteString.UTF8        as B (fromString, toString)
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           System.IO                   (hFlush, stdout)

-- | Lookup functions given in stdin in the dispatcher.
main :: IO ()
main = do printer <- newEmptyMVar
          forkIO . forever $ takeMVar printer >>= B.putStrLn >> hFlush stdout
          forever $
            do (f,n,line) <- extract <$> T.getLine
               xs         <- replicateM line T.getLine
               let r = run f n xs
               forkIO $ (r `using` rdeepseq) `seq` putMVar printer r

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
                      . parseOnly lisp . B.fromString . T.unpack

-- | Retrieves the contents of the result and annotates whether it was
-- a success.
failure :: Result B.ByteString -> B.ByteString
failure (Success s) = " yes)" <> s
failure (Error s)   = B.pack $ " nil)" ++ s

extract :: T.Text -> (T.Text, T.Text, Int)
extract = (\(x:y:z:_) -> (x, y, read $ T.unpack z)) . T.words

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.
run :: T.Text -> T.Text -> [T.Text] -> B.ByteString
run f n xs = B.concat
               [ "("
               , B.pack . show . length
                               . dropWhile (/=')')
                               . B.toString $ B.toStrict result
               , " "
               , B.pack $ T.unpack n
               , result
               ]
        where result = fromJust (M.lookup f dispatcher) $ T.unlines xs
