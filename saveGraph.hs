import System (getArgs)
import BinaryGraph
import TokyoGraph
import Database.TokyoCabinet
import System.IO

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip
import IntBS

eprintln s = do hPutStrLn stderr s
                hFlush stderr

main :: IO ()
main = do
  args  <-  getArgs
  let (fileName,saveBase,maxElems,progress) = 
        case args of 
          w:x:y:z:_ -> (w,x,Just (read y :: Int), Just (read z :: Int))
          w:x:y:_ -> (w,x,Just (read y :: Int),Just 10000)
          w:x:_ -> (w,x,Nothing,Just 10000)
          _ -> error "need a file name for the cabinet and a base to save"
  let ext = ".bin.zip"
      graphFile = saveBase ++ ".graph" ++ ext
      usersFile = saveBase ++ ".users" ++ ext
  eprintln ("reading graph from cabinet: " ++ fileName
    ++ "\n   saving graph in " ++ graphFile ++ ", users in " ++ usersFile
    ++ "\n   maxElems: " ++ (show maxElems) ++ ", progress: " ++ (show progress))
  -- (pack key) below for ByteString:
  (dic,graph) <- runTCM (fetchGraph fileName maxElems progress)
  -- println . show $ graph
  -- println (show . size $ graph)
  eprintln "well, let's save it now, shall we?"
  saveData graph graphFile
  saveData (trieIB dic)   usersFile
  