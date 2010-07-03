{-# LANGUAGE NoMonomorphismRestriction #-}

module BinaryGraph 
  ( loadData
  , printData
  , saveData
  , loadDataZ
  , saveDataZ
  , loadAnyData
  , saveAnyData
  ) where
  
-- import Graph
-- import IntBS
import System.FilePath.Posix
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip
-- also do BZip2, 7z if available
    
saveData = D.encodeFile
loadData = D.decodeFile
    
printData d =
  BL.putStr . compress . D.encode $ d

saveDataZ fileName d =
  BL.writeFile fileName $ compress . D.encode $ d 
  
loadDataZ :: D.Binary a => FilePath -> IO a
loadDataZ fileName = do
  bs <- BL.readFile fileName
  let g = D.decode . decompress $ bs
  return g
  
saveAnyData fileName =
  if takeExtension fileName == ".zip" then
    saveDataZ fileName
  else
    saveData fileName
    
loadAnyData fileName =
  if takeExtension fileName == ".zip" then
    loadDataZ fileName
  else
    loadData fileName
  

