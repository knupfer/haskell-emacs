{-# LANGUAGE OverloadedStrings #-}
--------------------
---- <<import>> ----
--------------------
import qualified Data.Map
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.AttoLisp as AL
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.UTF8 as BU

dispatcher :: Data.Map.Map T.Text (T.Text -> B.ByteString)
dispatcher = Data.Map.fromList
 [
 --------------------
 ---- <<export>> ----
 --------------------
 ]

transform :: (AL.FromLisp a, AL.ToLisp b) => (a -> b) -> T.Text -> B.ByteString
transform fu str = either (B.pack . (++) "FAIL:")
                          (failure . fmap (AL.encode . fu) . AL.fromLisp)
                          . AP.parseOnly AL.lisp . BU.fromString $ T.unpack str


failure :: AL.Result B.ByteString -> B.ByteString
failure (AL.Success a) = B.concat ["DONE:",a]
failure (AL.Error a ) = B.pack $ "FAIL:" ++ a
failure _ = B.pack "FAIL:unknown error"

main :: IO ()
main = do
    fun <- TIO.getLine
    case Data.Map.lookup fun dispatcher of
      Just function -> TIO.putStrLn "OK:" >> run function
      _ -> TIO.putStrLn (T.concat ["not found ", fun]) >> main

run :: (T.Text -> B.ByteString) -> IO ()
run fun = loop [""]
  where loop xs = do
         x <- TIO.getLine
         if x == "49e3524a756a100a5cf3d27ede74ea95"
            then B.putStrLn (fun . T.unlines $ reverse xs) >> main
            else loop (x:xs)
