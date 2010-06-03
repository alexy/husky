import System (getArgs)
import Database.TokyoCabinet
import Data.ByteString.Char8

-- runTCM $ do { db <- new :: TCM HDB; open db "foo.tch" [OWRITER, OCREAT]; put db "bar" "baz"; close db }

main :: IO ()
main = do 
  args <- getArgs
  tc <- new :: IO HDB
  let ext = ".hdb" -- defaultExtension tc
  let [file,key] = args
  v <- flip runTCM tc $ do
         open (file ++ ext) [OREADER]
         v <- get key
         close
         return v
--       `catchError` const (close >> fail "oops")
  print (v :: Maybe String)
