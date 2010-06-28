import System (getArgs)
import BinaryGraph
import TokyoGraph
import Database.TokyoCabinet
import System.IO

import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip
import qualified IntBS as IB
import IntBS (IntBS,backIB)

eprintln s = do hPutStrLn stderr s
                hFlush stderr

main :: IO ()
main = do
  args  <-  getArgs
  let (fileName,saveBase,users,maxElems,progress) = 
        case args of 
          v:w:x:y:z:_ -> (v,w,Just x,Just (read y :: Int), Just (read z :: Int))
          v:w:x:y:_ -> (v,w,Just x,Just (read y :: Int),Just 10000)
          v:w:x:_ -> (v,w,Just x,Nothing,Just 10000)
          v:w:_ -> (v,w,Nothing,Nothing,Just 10000)
          _ -> error "need a file name for the cabinet and a base to save"
  let ext = ".hsb.zip"
      graphFile = saveBase ++ ".int" ++ ext
      usersFile = saveBase ++ ".dic" ++ ext
  dic <- case users of
          Just fileName | (fileName /= "no") -> loadData fileName
          _ -> return IB.empty
  eprintln ("reading graph from cabinet: " ++ fileName
    ++ "\n  saving graph in " ++ graphFile ++ ", users in " ++ usersFile
    ++ "\n  " ++ case users of {Just x -> "merging users from " ++ x; _ -> "virginal users"}
    ++ "\n  maxElems: " ++ (show maxElems) ++ ", progress: " ++ (show progress))

  (dic,graph) <- runTCM (fetchGraph fileName dic maxElems progress)
  eprintln "well, let's save it now, shall we?"
  saveData graph graphFile
  saveData dic usersFile
  