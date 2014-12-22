{-# LANGUAGE OverloadedStrings #-}
---- <<import>> ----
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString as A (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8       as B (fromString)
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
transform f = either (B.pack . (++) "=:FAIL:=")
                      (failure . fmap (encode . f) . fromLisp)
                      . A.parseOnly lisp . B.fromString . T.unpack

-- | Retrieves the contents of the result and annotates whether it was
-- a success.
failure :: Result B.ByteString -> B.ByteString
failure (Success s) = "=:PASS:=\n" <> s
failure (Error s)   = B.pack $ "=:FAIL:=\n" ++ s

-- | Lookup functions given in stdin in the dispatcher.
main :: IO ()
main = do
    (f,n) <- fmap ((\(x:y:_) -> (x,y)) . T.words) T.getLine
    case M.lookup f dispatcher of
      Just function -> T.putStrLn "=:OK:="                      >> run function n
      _             -> T.putStr (T.unlines $ M.keys dispatcher) >> main

-- | Takes a function and feeds it stdin until all input is given and
-- prints the output.

run :: (T.Text -> B.ByteString) -> T.Text -> IO ()
run f n = loop []
  where loop xs = do
         x <- T.getLine
         if x == "49e3524a756a100a5cf3d27ede74ea95"
            then B.putStrLn (f . T.unlines $ reverse xs)
                >> T.putStr "=:PROC:="
                >> T.putStr n
                >> B.putStr "=:DONE:="
                >> hFlush stdout
                >> main
            else loop (x:xs)
