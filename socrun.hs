module SocRun where

import JSON2Graph (User, Day, Graph)
import Data.List (groupBy)
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.Map as M
import Data.Map ((!))
import Data.List (maximum)
import System.IO
import Debug.Trace

-- errln x = 
--   hPrint stderr x
--   hFlush stderr
  
type DCaps = M.Map User (M.Map Day Float)
type TalkBalance = M.Map User Int

emptyTalk :: TalkBalance
emptyTalk = M.empty

data UserStats = UserStats {
    socUS  :: Float, 
    dayUS  :: Int, 
    insUS  :: TalkBalance, 
    outsUS :: TalkBalance, 
    totUS  :: TalkBalance, 
    balUS  :: TalkBalance}
        
newUserStats :: Float -> Int -> UserStats
newUserStats soc day = UserStats {socUS = soc, dayUS = day, 
  insUS = emptyTalk, outsUS = emptyTalk, totUS = emptyTalk, balUS = emptyTalk}

type UStats = M.Map User UserStats
data SocRun = SocRun {alphaSR :: Float, betaSR :: Float, gammaSR :: Float, socInitSR :: Float}
optSocRun = SocRun 0.00001 0.5 0.5 1.0

data SGraph = SGraph {drepsSG :: Graph, dmentsSG :: Graph, dcapsSG :: DCaps, ustatsSG :: UStats}

paramSC (SocRun {alphaSR =a, betaSR =b, gammaSR =g}) = (a, b, g)

minMax1 (oldMin, oldMax) x =
  let newMin = oldMin `min` x
      newMax = oldMax `max` x in
      (newMin, newMax)

minMax2 (oldMin, oldMax) (x,y) =
  let newMin = oldMin `min` x
      newMax = oldMax `max` y in
      (newMin, newMax)

-- find the day range when each user exists in dreps
-- PRE: dreps must be sorted in adjacency lists by day!"
-- (assert (reps-sorted1? dreps))

dayRanges :: Graph -> M.Map User (Int, Int)
dayRanges dreps = M.map doDays dreps
  where
    doDays days =
      let (start, _) = M.elemAt 0 days -- i.e. "any map entry"     
          range = foldl' minMax1 (start, start) (M.keys days) in
          range

-- merge two day-ranges results
-- mergeDayRanges dr1 dr2 = M.unionWith min_max dr1 dr2

-- socRun :: Graph -> Graph -> SocRun -> IO ()
socRun dreps dments opts =      
    let 
    params  = paramSC opts 
    socInit = socInitSR opts
    dcaps   = M.empty -- TODO type
    ustats  = M.empty -- type
    sgraph  = SGraph dreps dments dcaps ustats
    dranges = M.toList $ M.unionsWith minMax2 (map dayRanges [dreps, dments])

    dstarts = M.fromList $ map dayUsers (groupBy ((==) `on` snd) dranges)
      where
        dayUsers g = let day = fst . snd . head $ g in (day, map fst g)
      
    -- elemAt 0 dstarts -- would return in insertion order, which is OK here too:
    firstDay = fst . head . M.toAscList $ dstarts  
    lastDay  = trace ("doing days from " ++ (show firstDay) ++ " to " ++ (show lastDay)) 
               maximum . map (snd . snd) $ dranges  
  
    
    tick sgraph day = 
    -- inject the users first appearing in this cycle
      let 
        ustats    = ustatsSG sgraph
        newUsers  = trace ("adding " ++ (show . length $ newUsers) ++ " new users on day " ++ (show day)) 
                    dstarts ! day
        newUstats = M.fromList $ map (\u -> (u,newUserStats socInit day)) newUsers 
        ustats'   = M.union ustats newUstats
        sgraph    = sgraph {ustatsSG = ustats'}
        in
        socDay sgraph params day

    in
      foldl' tick sgraph [firstDay..lastDay]

socDay sgraph params day = undefined