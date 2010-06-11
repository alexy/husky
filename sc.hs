import System (getArgs)
import qualified Data.Map as M
import Database.TokyoCabinet
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary as D
import Codec.Compression.GZip
import TokyoGraph
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let [drepsName,dmentsName] = args
  let eprintln = hPutStrLn stderr
  dreps <- loadGraph drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size dreps))
  dments <- loadGraph dmentsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size dreps))
  