import System (getArgs)
import BinaryGraph
import TokyoGraph
import Database.TokyoCabinet
import System.IO

main :: IO ()
main = do
  args  <-  getArgs
  let (fileName,maxElems,progress) = 
        case args of 
          x:y:z:_ -> (x,Just (read y :: Int), Just (read z :: Int))
          x:y:_ -> (x,Just (read y :: Int),Just 10000)
          x:_ -> (x,Nothing,Just 10000)
          _ -> error "need a file name for the cabinet"
  let eprintln = hPutStrLn stderr
  eprintln ("file: " ++ fileName ++ ", maxElems: " ++ (show maxElems) ++ ", progress: " ++ (show progress))
  -- (pack key) below for ByteString:
  graph <- runTCM (fetchGraph fileName maxElems progress)
  -- println . show $ graph
  -- println (show . size $ graph)
  eprintln "well, let's save it now, shall we?"
  saveGraph graph
  