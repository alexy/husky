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
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	", saving dcaps in " ++ saveName)
  dreps <- loadGraph drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  dments <- loadGraph dmentsName
  eprintln ("loaded " ++ dmentsName ++ ", " ++ (show . M.size $ dments))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  printGraph dcaps -- saveName
  