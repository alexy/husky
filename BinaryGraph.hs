module BinaryGraph (loadGraph,saveGraph) where
  
import Graph
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip

loadGraph :: FilePath -> IO Graph
loadGraph fileName = do
  bs <- BL.readFile fileName
  let g = D.decode . decompress $ bs :: Graph
  return g

-- sends to stdout
saveGraph :: Graph -> IO ()
saveGraph graph =
  BL.putStr (compress . D.encode $ graph)