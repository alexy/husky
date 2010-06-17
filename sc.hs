import System (getArgs)
import System.IO
import qualified Data.Map as M
import BinaryGraph
import SocRun
import Data.Maybe (listToMaybe)

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      maxDays = listToMaybe . map read $ restArgs
  dreps <- loadData drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  dments <- loadData dmentsName
  eprintln ("loaded " ++ dmentsName ++ ", " ++ (show . M.size $ dments))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun {maxDaysSR= maxDays}
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  -- printGraph dcaps
  saveData dcaps saveName