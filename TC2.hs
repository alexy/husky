import System (getArgs)
import Data.Map hiding (map)
import Database.TokyoCabinet
import qualified Data.ByteString.Char8 as B
import Text.JSONb
import JSON2Graph
import System.IO
import Control.Monad.Trans

fetch :: String -> Maybe Int -> Maybe Int -> TCM Graph    
fetch fileName maxElems progress = do
      tc <- new :: TCM HDB -- alternatively you can use BDB or FDB
      open tc fileName [OREADER]
      iterinit tc
      collect maxElems tc 0 []
    where 
      collect maxElems tc count acc = 
        let (haveMax,theMax) = case maxElems of {Just n -> (True,n); _ -> (False,0)} in 
        do
          k <- iternext tc
          case k of
              Just key | not haveMax || count < theMax -> do
                liftIO (case progress of
                          Just n | count `mod` n == 0 -> do
                            hPutChar stderr '.'
                            hFlush stderr
                          _ -> return ())
                v <- get tc key
                case v of
                  -- somehow things come back quote-escaped and quoted out of tokyo,
                  -- it's either clojure putting it or haskell getting it;
                  -- for now quickly unescape quotes and remove the enclosing ones
                  Just val -> -- let raw = B.filter (/='\\') . B.init . B.tail $ val in
                    collect maxElems tc (succ count) ((key,val):acc)
                  _ -> error "iternext has a key without a val"
              _ -> return (json2graph (reverse acc))
   

main :: IO ()
main = do
  args  <-  getArgs
  let (fileName,maxElems,progress) = 
        case args of 
          x:y:z:_ -> (x,Just (read y :: Int), Just (read z :: Int))
          x:y:_ -> (x,Just (read y :: Int),Just 10000)
          x:_ -> (x,Nothing,Just 10000)
          _ -> error "need a file name for the cabinet"
  let println = Prelude.putStrLn
  println ("file: " ++ fileName ++ ", maxElems: " ++ (show maxElems) ++ ", progress: " ++ (show progress))
  -- (pack key) below for ByteString:
  graph <- runTCM (fetch fileName maxElems progress)
  println . show $ graph
    