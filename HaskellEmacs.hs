{-# LANGUAGE OverloadedStrings #-}
---- <<import>> ----
import           Data.AttoLisp
import qualified Data.Attoparsec.ByteString as A (parseOnly)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8       as B (fromString)
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

dispatcher :: M.Map T.Text (T.Text -> B.ByteString)
dispatcher = M.fromList
 [
 ---- <<export>> ----
 ]

transform :: (FromLisp a, ToLisp b) => (a -> b) -> T.Text -> B.ByteString
transform fu str = either (B.pack . (++) "FAIL:")
                          (failure . fmap (encode . fu) . fromLisp)
                          . A.parseOnly lisp . B.fromString $ T.unpack str

failure :: Result B.ByteString -> B.ByteString
failure (Success a) = a
failure (Error a ) = B.pack $ "FAIL:" ++ a

main :: IO ()
main = do
    fun <- T.getLine
    case M.lookup fun dispatcher of
      Just function -> T.putStrLn "OK:" >> run function
      _ -> T.putStrLn (T.concat ["not found ", fun]) >> main

run :: (T.Text -> B.ByteString) -> IO ()
run fun = loop [""]
  where loop xs = do
         x <- T.getLine
         if x == "49e3524a756a100a5cf3d27ede74ea95"
            then B.putStrLn (fun . T.unlines $ reverse xs) >> main
            else loop (x:xs)
