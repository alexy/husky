import System (getArgs)
import System.IO
import qualified Data.IntMap as M
import SocRun
import Data.Maybe (listToMaybe)
import Graph
import Data.List (isSuffixOf)
import Database.TokyoCabinet (runTCM)
import TokyoGraph (fetchGraph)
import BinaryGraph
import qualified IntBS

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

-- may yse Sustem.FilePath.takeExtension repeatedly:
suffix = flip isSuffixOf

loadAnyGraph :: String -> IO Graph
loadAnyGraph fileName = 
  if suffix fileName ".hsb.zip" then
    loadData fileName
  else 
  if suffix fileName ".json.hdb" then do
    -- may dump dic on disk right there:
    (dic,graph) <- runTCM (fetchGraph fileName IntBS.empty Nothing (Just 10000))
    return graph
  else error "unrecognized graph file extension" 

main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      maxDays = listToMaybe . map read $ restArgs
  dreps <- loadAnyGraph  drepsName
  eprintln ("loaded " ++ drepsName ++ ", " ++ (show . M.size $ dreps))
  dments <- loadAnyGraph dmentsName
  eprintln ("loaded " ++ dmentsName ++ ", " ++ (show . M.size $ dments))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun {maxDaysSR= maxDays}
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  saveData dcaps saveName