{-# LANGUAGE OverloadedStrings #-}
--------------------
---- <<import>> ----
--------------------
import qualified Data.Map
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

dispatcher :: Data.Map.Map T.Text (T.Text -> T.Text)
dispatcher = Data.Map.fromList
 [
 --------------------
 ---- <<export>> ----
 --------------------
 ]

fromDataInt :: t
fromDataInt = undefined

fromDataMap :: Data.Map.Map a b -> T.Text
fromDataMap = undefined

fromDataSet :: Data.Map.Map a b -> T.Text
fromDataSet = undefined
-- oder doch mit attolisp parsen?
fromDataByteString :: Data.Map.Map a b -> T.Text
fromDataByteString = undefined

main :: IO ()
main = do
    fun <- TIO.getLine
    case Data.Map.lookup fun dispatcher of
      Just function -> TIO.putStrLn "OK" >> run function
      _ -> TIO.putStrLn (T.concat ["NOT FOUND: ", fun]) >> main

run :: (T.Text -> T.Text) -> IO ()
run fun = loop [""]
  where loop xs = do
         x <- TIO.getLine
         if x == "49e3524a756a100a5cf3d27ede74ea95"
            then TIO.putStr (fun $ T.unlines xs) >> main
            else loop (x:xs)
