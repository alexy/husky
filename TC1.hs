import System (getArgs)
import Database.TokyoCabinet
import Data.ByteString.Char8

-- getsample :: String -> ByteString -> TCM (Maybe ByteString)
getsample :: String -> String -> TCM (Maybe ByteString)
getsample file key =
   do tc <- new :: TCM HDB -- alternatively you can use BDB or FDB
      open tc file [OREADER]
      val <- get tc key
      close tc
      return val

main :: IO ()
main = do
  args  <-  getArgs
  let [file,key] = args
  let println = Prelude.putStrLn
  println ("file: "++file++", key: "++key)
  -- (pack key) below for ByteString:
  runTCM (getsample file key) >>=
    -- maybe (return ()) (println . show)  
    maybe (println "nothing") (println . unpack)