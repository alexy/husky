{-# LANGUAGE BangPatterns #-}
module SocRun (
  UserStats,
  DCaps,
  SGraph(..),
  SocRun, socRun, optSocRun
  )
-- TODO exports
where

import Graph
import Data.Ord (comparing)
import Data.List (groupBy,sortBy,foldl1')
import Data.Foldable (foldl')
import Data.Function (on)
import qualified Data.Map as M
import Data.Map ((!))
import Data.List (maximum)
import System.IO
import Debug.Trace
import Data.Maybe
import Control.Monad ((>=>))
--import Data.Nthable -- generalized fst
--import Prelude hiding (fst,snd)

-- errln x =
--   hPrint stderr x
--   hFlush stderr

type DCaps = M.Map User (M.Map Day Float)
type TalkBalance = M.Map User Int

emptyTalk :: TalkBalance
emptyTalk = M.empty

data UserStats = UserStats {
    socUS  :: !Float,
    dayUS  :: !Int,
    insUS  :: !TalkBalance,
    outsUS :: !TalkBalance,
    totUS  :: !TalkBalance,
    balUS  :: !TalkBalance}

newUserStats :: Float -> Int -> UserStats
newUserStats soc day = UserStats {socUS = soc, dayUS = day,
  insUS = emptyTalk, outsUS = emptyTalk, totUS = emptyTalk, balUS = emptyTalk}

type UStats = M.Map User UserStats
data SocRun = SocRun {alphaSR :: !Float, betaSR :: !Float, gammaSR :: !Float, socInitSR :: !Float}
optSocRun = SocRun 0.00001 0.5 0.5 1.0

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

dayRanges :: Graph -> M.Map User (Int, Int)
dayRanges dreps = M.map doDays dreps
  where
    doDays days =
      let (!start, _) = M.elemAt 0 days -- i.e. "any map entry"
          range@(!mi,!ma) = foldl' minMax1 (start, start) (M.keys days) in
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
      getFirstDay = fst . snd
      dranges = let x = sortBy (comparing getFirstDay) . M.toList $ M.unionsWith minMax2 (map dayRanges [dreps, dments]) in
                  trace ("dranges has length " ++ (show . length $ x)) x

      !dstarts = M.fromList $ map dayUsers (groupBy ((==) `on` getFirstDay) dranges)
        where
          dayUsers g = let !day = getFirstDay . head $ g in (day, map fst g)

      -- elemAt 0 dstarts -- would return in insertion order, which is OK here too:
      !firstDay = fst . head . M.toAscList $ dstarts
      !lastDay  = let x = maximum . map (snd . snd) $ dranges in
                   trace ("doing days from " ++ (show firstDay) ++ " to " ++ (show x))
                   x


      tick sgraph day =
      -- inject the users first appearing in this cycle
        let
          ustats    = ustatsSG sgraph
          newUsers  = let x = dstarts ! day in
                        trace ("adding " ++ (show . length $ x) ++ " new users on day " ++ (show day))
                        x
          !newUstats = M.fromList $ map (\u -> (u,newUserStats socInit day)) newUsers
          ustats'   = M.union ustats newUstats
          !sgraph'   = sgraph {ustatsSG = ustats'}
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
    (alpha, beta, gamma) = params
    SGraph {ustatsSG =ustats, dcapsSG =dcaps} = sgraph
    users = M.keys ustats

    termsStats = map (socUserDaySum sgraph day) users
    sumTerms   = -- trace ("got sumTerms, length " ++ (show . length $ sumTerms))
                 map fst termsStats

    -- norms = foldl1' (zipWith (+)) sumTerms
    norms = foldl1' (\(!x,!y,!z) (x',y',z') -> (x+x',y+y',z+z')) (catMaybes sumTerms)

    tick user (numers,stats) =
      let
        soc = socUS stats
        !soc' =
          case numers of
            Just numers ->
              let (outs', insBack', insAll') =
                   -- map safeDivide numers norms
                   safeDivide3 numers norms
              in
              alpha * soc + (1 - alpha) *
                (beta * outs' + (1 - beta) *
                  (gamma * insBack' + (1 - gamma) * insAll'))
            Nothing -> alpha * soc
        stats' =  stats {socUS = soc'}
        in
        (user,stats')

    !ustats' = -- trace "got ustats"
              M.fromList $ zipWith tick users termsStats

    -- day in fn is the same day as soc-day param day
    -- TODO fold[l/r]WithKey?
    !dcaps' = M.foldWithKey updateUser dcaps ustats'
      where
        updateUser user userStats res =
          let UserStats{dayUS =day, socUS =soc} = userStats
              -- TODO the equations differ only in the tail map, can simplify
              -- addDay (Just days) = Just (M.insert day soc days)
              -- addDay _ = Just (M.insert day soc M.empty) -- M.fromList [(day,soc)]
              -- M.singleton is shorter still, but this is real good, all by kmc:
              addDay m = let !res = M.insert day soc $ fromMaybe M.empty m in Just res
          in
          M.alter addDay user res
    in
    sgraph {ustatsSG= ustats', dcapsSG= dcaps'}

-- socUserDaySum sgraph day user = undefined

getUserDay user day = M.lookup user >=> M.lookup day

-- we started writing this and BMeph finished, but Map has findWithDefault already:
-- lookupWithDefault d = fromMaybe d . flip M.lookup

getSoccap ustats user =
  case M.lookup user ustats of
    Just UserStats{socUS =soc} -> soc
    _ -> 0

socUserDaySum sgraph day user =
  let
    SGraph {drepsSG =dreps, dmentsSG =dments, ustatsSG =ustats} = sgraph
    !stats = ustats ! user
    dr_ = getUserDay user day dreps
    dm_ = getUserDay user day dments
    in

    if not (isJust dr_ || isJust dm_) then
        (Nothing, stats)
    else
      -- we had edges this cycle -- now let's dance and compute the change!
      let
        UserStats {socUS =soc, dayUS =day, insUS =ins, outsUS =outs, totUS =tot, balUS =bal} = stats

        -- changing order from foldlWithKey to foldWithKey's
        -- to be able to run under GHC 6.10
        socStep pred to num res =
          let toBal = M.findWithDefault 0 to bal in
          if not (pred toBal) then 0
          else
            let toSoc = getSoccap ustats to in
              if toSoc == 0 then 0
              else
                let
                  toTot = M.findWithDefault 1 to tot
                  !term = fromIntegral (num * toBal * toTot) * toSoc
                  in
                  res + term

        -- find all those who talked to us in the past to whom we replied now
        !outSum =
          case dr_ of
            Nothing -> 0
            Just dr ->
              M.foldWithKey (socStep (<0)) 0 dr


        !inSumBack =
          case dm_ of
            Nothing -> 0
            Just dm ->
              M.foldWithKey (socStep (>0)) 0 dm

        !inSumAll =
          case dm_ of
            Nothing -> 0
            Just dm ->
              M.foldWithKey step 0 dm
              where
                step to num res =
                  let toSoc = getSoccap ustats to in
                    if toSoc == 0 then 0
                    else
                      let
                        toTot = M.findWithDefault 1 to tot
                        !term = fromIntegral (num * toTot) * toSoc
                        in
                        res + term

        terms = (outSum, inSumBack, inSumAll)

        addMaps      = M.unionWith (+)
        subtractMaps = M.unionWith (-)

        !ins'  = case dr_ of {Just dr -> addMaps ins dr;  _ -> ins}
        !outs' = case dm_ of {Just dm -> addMaps outs dm; _ -> outs}

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
