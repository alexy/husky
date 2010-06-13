module TokyoGraph (fetchGraph) where
  
import Database.TokyoCabinet
import Graph
import JSON2Graph
import Control.Monad.Trans -- liftIO
import System.IO

fetchGraph :: FilePath -> Maybe Int -> Maybe Int -> TCM Graph    
fetchGraph fileName maxElems progress = do
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
   