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

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

-- may use Sustem.FilePath.takeExtension repeatedly:
suffix = flip isSuffixOf

-- we have to do both to thread dic through tokyos
-- there gotta be some monadic way to do with one and combine
-- loadAnyGraph :: String -> String -> String -> IO (Graph, Graph, IntMapBS, Timings)
loadAnyGraph :: String -> String -> String -> IO (Graph, Graph, IntBS, Timings)
loadAnyGraph f1 f2 dicName = 
  if suffix f1 ".json.hdb" 
    then do
      -- may dump dic on disk right there:
      -- TODO there gotta be some monadic gymnastics for that!
      (!dic,!g1) <- runTCM (fetchGraph f1 IntBS.empty Nothing (Just 10000))
      t1 <- getTiming $ Just "loading json dreps timing: "
      (!dic',!g2) <- runTCM (fetchGraph f2 dic        Nothing (Just 10000))
      t2 <- getTiming $ Just "loading json dments timing: "
      saveAnyData dicName dic'
      t3 <- getTiming $ Just "saving dic timing: "
      eprintln "saved the user<=>int dictionary"
      return (g1, g2, dic', [t3,t2,t1])
    else do
      !g1 <- loadAnyData f1
      t1 <- getTiming $ Just "loading binary dreps timing: "
      !g2 <- loadAnyData f2
      t2 <- getTiming $ Just "loading binary dments timing: "
      -- we load only the IntMap part of IntBS here, which is stored first
      let gotDic = dicName == "none"
      dic <- if gotDic then return IntBS.empty else loadData dicName
      t3 <- getTiming $ if gotDic then Just "loading dic timing: " else Nothing
      return (g1, g2, dic, [t3,t2,t1])
  
main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:dicName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	" user-int dictionary in " ++ dicName ++ ", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      !maxDays = listToMaybe . map read $ restArgs
  (!dreps, !dments, !dic, t0) <- loadAnyGraph drepsName dmentsName dicName
  
  eprintln ("loaded dreps from "   ++ drepsName  ++ ", " ++ (show . IM.size $ dreps))
  eprintln ("loaded dments from "  ++ dmentsName ++ ", " ++ (show . IM.size $ dments))
  eprintln ("using dictionary in " ++ dicName    ++ ", " ++ (show . totalIB $ dic))
  
  (sgraph,t1) <- socRun dreps dments optSocRun {maxDaysSR= maxDays}
  let !dcaps = dcapsSG sgraph
      
  eprintln "computed sgraph"
  t2 <-  
    if dicName == "none" 
      then do
        eprintln ("saving int dcaps in " ++ saveName)
        t2 <- getTiming Nothing
        saveAnyData saveName dcaps
        return t2
      else do
        eprintln "disinterning dcaps"
        -- TODO !dcaps' takes longer?
        -- disintern uses IntMap part, disintern2 uses Trie
        if isInfixOf ".map" saveName 
          then do
            let !dcaps' = disintern2map1 dic dcaps
            eprintln ("saving map string dcaps in " ++ saveName)
            saveAnyData saveName dcaps'
          else do -- check for isInfixOf ".trie" saveName?
            let !dcaps' = disintern2trie1 dic dcaps
            eprintln ("saving trie string dcaps in " ++ saveName)
            saveAnyData saveName dcaps'
        t2 <- getTiming $ Just "disinterning dcaps timing: "
        return t2
  t3 <- getTiming $ Just "saving dcaps timing: "
  
  let ts = reverse $ [t3,t2] ++ t1 ++ t0
  eprintln ("timings: " ++ show ts)
