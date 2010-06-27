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

loadAnyGraph :: String -> String -> IO (Graph, Graph)
loadAnyGraph f1 f2 = 
  if suffix f1 ".hsb.zip" then do
    g1 <- loadData f1
    g2 <- loadData f2
    return (g1,g2)
  else 
  if suffix f1 ".json.hdb" then do
    -- may dump dic on disk right there:
    -- TODO there gotta be some monadic gymnastics for that!
    (dic,g1) <- runTCM (fetchGraph f1 IntBS.empty Nothing (Just 10000))
    (dic,g2) <- runTCM (fetchGraph f2 dic         Nothing (Just 10000))
    return (g1,g2)
  else error "unrecognized graph file extension" 

main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      maxDays = listToMaybe . map read $ restArgs
  (dreps, dments) <- loadAnyGraph drepsName dmentsName
  eprintln ("loaded " ++ drepsName  ++ ", " ++ (show . M.size $ dreps))
  eprintln ("loaded " ++ dmentsName ++ ", " ++ (show . M.size $ dments))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun {maxDaysSR= maxDays}
  eprintln ("computed sgraph, now saving dcaps in " ++ saveName)
  saveData dcaps saveName