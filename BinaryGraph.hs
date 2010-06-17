module BinaryGraph (loadData,printData,saveData) where
  
-- import Graph
-- import IntBS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip

printData d =
  BL.putStr . compress . D.encode $ d
    
saveData d fileName =
  BL.writeFile fileName $ compress . D.encode $ d 
  
loadData :: D.Binary a => FilePath -> IO a
loadData fileName = do
  bs <- BL.readFile fileName
  let g = D.decode . decompress $ bs
  return g

