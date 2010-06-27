{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import System.IO
import qualified Data.IntMap as IM
import Data.IntMap ((!))
import Utils (foldWithKey')
import qualified Data.Map as M
import SocRun
import Data.Maybe (listToMaybe)
import Graph
import Data.List (isSuffixOf)
import Database.TokyoCabinet (runTCM)
import TokyoGraph (fetchGraph)
import BinaryGraph
import qualified IntBS
import IntBS (IntBS(..))

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

-- may yse Sustem.FilePath.takeExtension repeatedly:
suffix = flip isSuffixOf

-- we have to do both to thread dic through tokyos
-- there gotta be some monadic way to do with one and combine
loadAnyGraph :: String -> String -> String -> IO (Graph, Graph, IntBS)
loadAnyGraph f1 f2 dicName = 
  if suffix f1 ".hsb.zip" then do
    g1  <- loadData f1
    g2  <- loadData f2
    dic <- if dicName == "none" then return IntBS.empty else loadData dicName
    return (g1,g2,dic)
  else 
  if suffix f1 ".json.hdb" then do
    -- may dump dic on disk right there:
    -- TODO there gotta be some monadic gymnastics for that!
    (dic,g1) <- runTCM (fetchGraph f1 IntBS.empty Nothing (Just 10000))
    (dic',g2) <- runTCM (fetchGraph f2 dic        Nothing (Just 10000))
    saveData dic' dicName
    eprintln "saved the user<=>int dictionary"
    return (g1,g2,dic')
  else error "unrecognized graph file extension" 


-- this is not strict enough, the thing explodes
-- might as well disintern into a Trie instead
disintern dic =
  let ib = backIB dic in
  -- \!k was a syntax error... on ->
  foldWithKey' (\ !k !v !res -> let !name = ib ! k in M.insert name v res) M.empty 
  
main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:dicName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	" user-int dictionary in " ++ dicName ++ ", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      maxDays = listToMaybe . map read $ restArgs
  (!dreps, !dments, !dic) <- loadAnyGraph drepsName dmentsName dicName
  eprintln ("loaded dreps from " ++ drepsName  ++ ", " ++ (show . IM.size $ dreps))
  eprintln ("loaded dments from " ++ dmentsName ++ ", " ++ (show . IM.size $ dments))
  eprintln ("using dictionary in " ++ dicName ++ ", " ++ (show . totalIB $ dic))
  
  let SGraph{dcapsSG =dcaps} = socRun dreps dments optSocRun {maxDaysSR= maxDays}
  eprintln "computed sgraph"
  if dicName == "none" 
    then do
      eprintln ("saving int dcaps in " ++ saveName)
      saveData dcaps saveName
    else do
      eprintln "disinterning dcaps"
      let dcaps' = disintern dic dcaps
      eprintln ("saving string dcaps in " ++ saveName)
      saveData dcaps' saveName
