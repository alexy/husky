import System (getArgs)
import System.IO
import qualified Data.Map as M
import BinaryGraph
import SocRun

main :: IO ()
main = do
  args <- getArgs
  let [drepsName,dmentsName,saveName] = args
  let eprintln = hPutStrLn stderr -- can add hFlush stderr as well
  dreps <- loadGraph drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  dments <- loadGraph dmentsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  saveGraph dcaps saveName
  