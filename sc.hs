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
import Data.List (isSuffixOf)
import Database.TokyoCabinet (runTCM)
import TokyoGraph (fetchGraph)
import BinaryGraph
import qualified IntBS
import IntBS (IntBS(..),IntMapBS)

eprintln s = do
	hPutStrLn stderr s
	hFlush stderr

-- may use Sustem.FilePath.takeExtension repeatedly:
suffix = flip isSuffixOf

-- we have to do both to thread dic through tokyos
-- there gotta be some monadic way to do with one and combine
loadAnyGraph :: String -> String -> String -> IO (Graph, Graph, IntMapBS,Timings)
loadAnyGraph f1 f2 dicName = 
  if suffix f1 ".hsb.zip" then do
    let gotDic = dicName == "none"
    !g1 <- loadData f1
    t1 <- getTiming $ Just "loading binary dreps timing: "
    !g2 <- loadData f2
    t2 <- getTiming $ Just "loading binary dments timing: "
    -- we load only the IntMap part of IntBS here, which is stored first
    dicIB <- if gotDic then return IM.empty else loadData dicName
    t3 <- getTiming $ if gotDic then Just "loading dic timing: " else Nothing
    return (g1, g2, dicIB, [t3,t2,t1])
  else 
  if suffix f1 ".json.hdb" then do
    -- may dump dic on disk right there:
    -- TODO there gotta be some monadic gymnastics for that!
    (!dic,!g1) <- runTCM (fetchGraph f1 IntBS.empty Nothing (Just 10000))
    t1 <- getTiming $ Just "loading json dreps timing: "
    (!dic',!g2) <- runTCM (fetchGraph f2 dic        Nothing (Just 10000))
    t2 <- getTiming $ Just "loading json dments timing: "
    saveData dic' dicName
    t3 <- getTiming $ Just "saving dic timing: "
    eprintln "saved the user<=>int dictionary"
    return (g1, g2, backIB dic', [t3,t2,t1])
  else error "unrecognized graph file extension" 


-- this is not strict enough, the thing explodes
-- how can we ensure M.insert stays strict?
-- might as well disintern into a Trie instead
-- TODO: Cale suggested using builder for toAscList
-- on #haskell circa 2010-06-22 -- see Utils.hs

-- or might disintern into a Trie to lookup faster:

disintern !ib =
 IM.foldWithKey step M.empty
   where
     step !k !v !res = {-# SCC "disintern.step" #-} case ib ! k of
                          !name -> M.insert name v res

  
main :: IO ()
main = do
  args <- getArgs
  let drepsName:dmentsName:dicName:saveName:restArgs = args
  eprintln ("reading graph from " ++ drepsName ++ ", " ++ dmentsName ++ 
  	" user-int dictionary in " ++ dicName ++ ", saving dcaps in " ++ saveName)
  let maxDays :: Maybe Int 
      !maxDays = listToMaybe . map read $ restArgs
  (!dreps, !dments, !dicIB, t0) <- loadAnyGraph drepsName dmentsName dicName
  
  eprintln ("loaded dreps from "   ++ drepsName  ++ ", " ++ (show . IM.size $ dreps))
  eprintln ("loaded dments from "  ++ dmentsName ++ ", " ++ (show . IM.size $ dments))
  eprintln ("using dictionary in " ++ dicName    ++ ", " ++ (show . IM.size $ dicIB))
  
  (sgraph,t1) <- socRun dreps dments optSocRun {maxDaysSR= maxDays}
  let !dcaps = dcapsSG sgraph
      
  eprintln "computed sgraph"
  t2 <-  
    if dicName == "none" 
      then do
        eprintln ("saving int dcaps in " ++ saveName)
        t2 <- getTiming Nothing
        saveData dcaps saveName
        return t2
      else do
        eprintln "disinterning dcaps"
        -- TODO !dcaps' takes longer?
        let !dcaps' = disintern dicIB dcaps
        t2 <- getTiming $ Just "disinterning dcaps timing: "
        eprintln ("saving string dcaps in " ++ saveName)
        saveData dcaps' saveName
        return t2
  t3 <- getTiming $ Just "saving dcaps timing: "
  
  let ts = reverse $ [t3,t2] ++ t1 ++ t0
  eprintln ("timings: " ++ show ts)
