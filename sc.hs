{-# LANGUAGE BangPatterns #-}

import System (getArgs)
import System.IO
import qualified IntMap as IM
import IntMap ((!))
import Utils (Timings,getTiming)
import qualified Data.Map as M
import SocRun
import Data.Maybe (listToMaybe)
import Graph
import Data.List (isSuffixOf,isInfixOf)
import Database.TokyoCabinet (runTCM)
import TokyoGraph (fetchGraph)
import BinaryGraph
import qualified IntBS
import IntBS (IntBS(..),IntMapBS)
import Intern (disintern2trie1,disintern2map1)
import Invert

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

-- we have to do both to thread dic through tokyos
-- there gotta be some monadic way to do with one and combine
-- loadAnyGraph :: String -> String -> String -> IO (Graph, Graph, IntMapBS, Timings)
loadAnyGraph :: String -> String -> IO (Graph, IntBS, Timings)
loadAnyGraph drepsName dicName = 
  if ".json.hdb" `isSuffixOf` drepsName
    then do
      -- may dump dic on disk right there:
      -- TODO there gotta be some monadic gymnastics for that!
      (!dic,!g) <- runTCM (fetchGraph drepsName IntBS.empty Nothing (Just 10000))
      t1 <- getTiming $ Just "loading json dreps timing: "
      saveAnyData dicName dic
      t2 <- getTiming $ Just "saving dic timing: "
      eprintln "saved the user<=>int dictionary"
      return (g, dic, [t2,t1])
    else do
      !g <- loadAnyData drepsName
      t1 <- getTiming $ Just "loading binary dreps timing: "
      let gotDic = dicName == "none"
      dic <- if gotDic then return IntBS.empty else loadAnyData dicName
      t2 <- getTiming $ if gotDic then Just "loading dic timing: " else Nothing
      return (g, dic, [t2,t1])
  
main :: IO ()
main = do
  args <- getArgs
  let drepsName:dicName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ 
  	" with user<=>int dictionary from " ++ dicName ++ ", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      !maxDays = listToMaybe . map read $ restArgs
  (!dreps, !dic, tLoad) <- loadAnyGraph drepsName dicName
  let !dments = invert1 dreps  
  tInvert <- getTiming $ Just "inverting into dments: "
        
  eprintln ("loaded dreps from "   ++ drepsName  ++ ", " ++ (show . IM.size $ dreps))
  eprintln ("inverted into dments, "  ++ (show . IM.size $ dments))
  eprintln ("using dictionary in " ++ dicName    ++ ", " ++ (show . totalIB $ dic))
  
  (sgraph,tSocRun) <- socRun dreps dments optSocRun {maxDaysSR= maxDays}
  let !dcaps = dcapsSG sgraph
      
  eprintln "computed sgraph"
  tDisintern <-  
    if dicName == "none" 
      then do
        eprintln ("saving int dcaps in " ++ saveName)
        saveAnyData saveName dcaps
        getTiming Nothing
      else do
        eprintln "disinterning dcaps"
        -- TODO !dcaps' takes longer?
        -- disintern uses IntMap part, disintern2 uses Trie
        if ".map" `isInfixOf` saveName 
          then do
            let !dcaps' = disintern2map1 dic dcaps
            eprintln ("saving map string dcaps in " ++ saveName)
            saveAnyData saveName dcaps'
          else do -- check for isInfixOf ".trie" saveName?
            let !dcaps' = disintern2trie1 dic dcaps
            eprintln ("saving trie string dcaps in " ++ saveName)
            saveAnyData saveName dcaps'
        getTiming $ Just "disinterning dcaps timing: "
  tSaving <- getTiming $ Just "saving dcaps timing: "
  
  let ts = reverse $ [tSaving,tDisintern] ++ tSocRun ++ [tInvert] ++ tLoad
  eprintln ("timings: " ++ show ts)
