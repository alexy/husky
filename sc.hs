import System (getArgs)
import System.IO
import qualified Data.Map as M
import BinaryGraph
import SocRun

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

main :: IO ()
main = do
  args <- getArgs
  let [drepsName,dmentsName,saveName] = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	", saving dcaps in " ++ saveName)
  dreps <- loadGraph drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  dments <- loadGraph dmentsName
  eprintln ("loaded " ++ dmentsName ++ ", " ++ (show . M.size $ dments))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  printGraph dcaps
  -- saveGraph dcaps saveName