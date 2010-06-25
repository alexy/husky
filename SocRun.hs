{-# LANGUAGE BangPatterns #-}


module SocRun (
  UserStats,
  DCaps,
  SGraph(..),
  SocRun(..), socRun, optSocRun
  )
-- TODO exports
where

import Graph
import Data.Ord (comparing)
import Data.List (groupBy,sortBy,foldl1')
import Data.Function (on)
import qualified Data.IntMap as M
import Data.IntMap ((!))
-- is there a difference between foldl' from Foldable or List?
-- import Data.Foldable (foldl')
import Data.List (maximum,foldl')
import System.IO
import Debug.Trace
import Data.Maybe
import Control.Monad ((>=>))
--import Data.Nthable -- generalized fst
--import Prelude hiding (fst,snd)

-- errln x =
--   hPrint stderr x
--   hFlush stderr

type DCaps = M.IntMap (M.IntMap Float)
type TalkBalance = M.IntMap Int

emptyTalk :: TalkBalance
emptyTalk = M.empty

data UserStats = UserStats {
    socUS  :: !Float,
    dayUS  :: !Int,
    insUS  :: TalkBalance,
    outsUS :: TalkBalance,
    totUS  :: TalkBalance,
    balUS  :: TalkBalance}

newUserStats :: Float -> Int -> UserStats
newUserStats soc day = UserStats {socUS = soc, dayUS = day,
  insUS = emptyTalk, outsUS = emptyTalk, totUS = emptyTalk, balUS = emptyTalk}

type UStats = M.IntMap UserStats
data SocRun = SocRun {alphaSR :: !Float, betaSR :: !Float, gammaSR :: !Float,
                      socInitSR :: !Float, maxDaysSR :: Maybe Int}
optSocRun = SocRun 0.00001 0.5 0.5 1.0 Nothing

data SGraph = SGraph {drepsSG :: !Graph, dmentsSG :: !Graph, dcapsSG :: !DCaps, ustatsSG :: !UStats}

paramSC (SocRun {alphaSR =a, betaSR =b, gammaSR =g}) = (a, b, g)

minMax1 (oldMin, oldMax) x =
  let !newMin = oldMin `min` x
      !newMax = oldMax `max` x in
      (newMin, newMax)

minMax2 (oldMin, oldMax) (x,y) =
  let !newMin = oldMin `min` x
      !newMax = oldMax `max` y in
      (newMin, newMax)

-- find the day range when each user exists in dreps
-- PRE: dreps must be sorted in adjacency lists by day!"
-- (assert (reps-sorted1? dreps))

dayRanges :: Graph -> M.IntMap (Int, Int)
dayRanges dreps = M.map doDays dreps
  where
    doDays days = case fst $ M.findMin days of
                    !f -> case fst $ M.findMax days of
                            !l -> (f, l)
--       let (!start, _) = M.elemAt 0 days -- i.e. "any map entry"
--           range@(!f,!l) = foldl' minMax1 (start, start) (M.keys days) in
--           range

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
      presMap = M.unionWith minMax2 (dayRanges dreps) (dayRanges dments)
      !l0 = snd . snd $ M.findMax presMap
      (!lastDay0, !dstarts) = foldl' updt (l0, M.empty) (M.assocs presMap)
      -- Data.Map has insertWith' which we used below, but Data.IntMap doesn't:
      -- updt (!ld, !m) (u,(f,l)) = (max ld l, M.insertWith' (++) f [u] m)
      updt (!ld, !m) (u,(f,l)) = (max ld l, M.insertWith (++) f [u] m)
      !firstDay = fst $ M.findMin dstarts
      !lastDay  = let x' = maybe lastDay0 (\y -> min lastDay0 (firstDay + y - 1)) (maxDaysSR opts)
      			      in
                    trace ("doing days from " ++ (show firstDay) ++ " to " ++ (show x'))
                    x'


      tick sgraph day =
      -- inject the users first appearing in this cycle
        let
          nus       = newUserStats socInit day
          !ustats    = ustatsSG sgraph
          newUsers  = let x = dstarts ! day in
                        trace ("adding " ++ (show . length $ x) ++ " new users on day " ++ (show day))
                        x
          !ustats' = foldl' insn ustats newUsers
          insn m u = M.insert u nus m
          !sgraph'   = trace ("now got " ++ show (M.size ustats')) sgraph {ustatsSG = ustats'}
        in
          socDay sgraph' params day

    in
      foldl' tick sgraph [firstDay..lastDay]

-- socDay sgraph params day = undefined

safeDivide :: (Fractional a) => a -> a -> a
safeDivide x 0 = x
safeDivide x y = x / y

-- fst3 (x,_,_) = x

safeDivide3 (x,y,z) (x',y',z') =
  let
    !a = safeDivide x x'
    !b = safeDivide y y'
    !c = safeDivide z z'
  in (a,b,c)

socDay sgraph params day =
  let
    (!alpha, !beta, !gamma) = params
    --SGraph {ustatsSG =ustats, dcapsSG =dcaps} = sgraph
    !ustats = ustatsSG sgraph
    !dcaps  = dcapsSG sgraph
    -- users = M.keys ustats

    -- my bangs, needed?
    tsmap u _ = case socUserDaySum sgraph day u of
                 (Nothing,st) -> st `seq` (Nothing,st)
                 (Just x, st) -> x `seq` st `seq` (Just x, st)
    !termsStats = {-# SCC "termsStats" #-} M.mapWithKey tsmap ustats
   -- !termsStats = M.mapWithKey (const . socUserDaySum sgraph day) ustats
    sumTerms   = catMaybes . map fst . M.elems $ termsStats

    -- norms = foldl1' (zipWith (+)) sumTerms
    norms@(!a,!b,!c) = {-# SCC "norms" #-} foldl1' (\(!x,!y,!z) (!x',!y',!z') -> (x+x',y+y',z+z')) sumTerms

    tick (!numers,!stats) = {-# SCC "socDay.tick" #-}
      let
        !soc = socUS stats
        !soc' =
          case numers of
            Just numers ->
              let (!outs', !insBack', !insAll') =
                   -- map safeDivide numers norms
                   safeDivide3 numers norms
              in
              alpha * soc + (1 - alpha) *
                (beta * outs' + (1 - beta) *
                  (gamma * insBack' + (1 - gamma) * insAll'))
            Nothing -> alpha * soc
        !stats' =  stats {socUS = soc'}
        in
        stats'

    -- TODO verify replacement of M.intersectionWithKey by just M.map,
    -- and dropping the leading two parameters from tick above
    !ustats' = {-# SCC "ustats'" #-} M.map tick termsStats

    -- TODO: @dafis strictified this, but the logic needs checking
    !dcaps' = {-# SCC "dcaps'" #-} M.foldWithKey updateUser dcaps ustats'
      where
        updateUser !user !stats !res =
          case socUS stats of
            -- IntMap has no insertWith', so we revert to insertWith:
            -- TODO just keep it as a list and append instead of maps:
            !soc -> M.insertWith (flip M.union) user (M.singleton day soc) res
    in
    sgraph {ustatsSG= ustats', dcapsSG= dcaps'}

-- socUserDaySum sgraph day user = undefined

--getUserDay user day = M.lookup user >=> M.lookup day
{-# INLINE getUserDay #-}
getUserDay user day m =
      case {-# SCC "getUserDay.user" #-} M.lookup user m of
        Just m' -> {-# SCC "getUserDay.day" #-} M.lookup day m'
        Nothing -> Nothing

-- we started writing this and BMeph finished, but Map has findWithDefault already:
-- lookupWithDefault d = fromMaybe d . flip M.lookup

getSocCap ustats user =
  case M.lookup user ustats of
    Just UserStats{socUS =soc} -> soc
    _ -> 0

-- our strict version of M.foldWithKey
-- the step function obeys the parent's param order
foldWithKey' :: (Int -> a -> b -> b) -> b -> M.IntMap a -> b
foldWithKey' f z m = foldl' step z (M.toList m)
  where step res (k,v) = f k v res
          
socUserDaySum sgraph day user =
  let
    SGraph {drepsSG =dreps, dmentsSG =dments, ustatsSG =ustats} = sgraph
    !stats = {-# SCC "socUDSum.stats" #-} ustats ! user
    !dr_ = getUserDay user day dreps
    !dm_ = getUserDay user day dments
    in

    if not (isJust dr_ || isJust dm_) then
        (Nothing, stats)
    else
      -- we had edges this cycle -- now let's dance and compute the change!
      let
        UserStats {socUS =soc, dayUS =day, insUS =ins, outsUS =outs, totUS =tot, balUS =bal} = stats

        -- find all those who talked to us in the past to whom we replied now
        !outSum = {-# SCC "outSum" #-}
          case dr_ of
            Nothing -> 0
            Just dr ->
              foldWithKey' step 0 dr
              where
                 step !to !num !res = {-# SCC "outStep" #-}
                  let !toBal = M.findWithDefault 0 to bal in
                  if toBal >= 0 then 0
                  else
                    let !toSoc = getSocCap ustats to in
                      if toSoc == 0 then 0
                      else
                        let
                          toTot = M.findWithDefault 1 to tot
                          !term = fromIntegral (num * toBal * toTot) * toSoc
                          in
                          res + term
               

        (!inSumBack,!inSumAll) = {-# SCC "inSumBack" #-}
          case dm_ of
            Nothing -> (0,0)
            Just dm ->
              foldWithKey' step (0,0) dm
              where
                 step !to !num (!backSum,!allSum) = {-# SCC "inStep" #-}
                  let 
                    !toBal = M.findWithDefault 0 to bal
                    !toSoc = getSocCap ustats to in
                    if toSoc == 0 then (0,0)
                    else
                      let
                        toTot = M.findWithDefault 1 to tot
                        !allTerm  = fromIntegral (num * toTot) * toSoc
                        -- TODO: corrected by iffing cases
                        !backTerm = if toBal <= 0 then 0 else fromIntegral toBal * allTerm
                        in
                        (backSum + backTerm,allSum + allTerm)


        terms = (outSum, inSumBack, inSumAll)

        addMaps      = M.unionWith (+)
        subtractMaps = M.unionWith (-)

        ins'  = case dr_ of {Just dr -> addMaps ins dr;  _ -> ins}
        outs' = case dm_ of {Just dm -> addMaps outs dm; _ -> outs}

        -- ziman: M.unionWith (+) `on` maybe M.empty id
        (tot', bal')  =
          case (dr_, dm_) of
            (Just dr, Nothing) -> (addMaps tot dr, addMaps bal dr)
            (Nothing, Just dm) -> (addMaps tot dm, subtractMaps bal dm)
            (Just dr, Just dm) ->
              let t = addMaps dr $ addMaps tot dm
                  b = addMaps dr $ subtractMaps bal dm
              in
              (t,b)

        !stats' = stats {insUS= ins', outsUS= outs', totUS= tot', balUS= bal'}
        in
        (Just terms, stats')
