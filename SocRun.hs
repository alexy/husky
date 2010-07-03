{-# LANGUAGE BangPatterns #-}

module SocRun (
  UserStats,
  DCaps,
  SGraph(..),
  SocRun(..), socRun, optSocRun
  )
where

import Graph
import Utils (Timings,getTiming)
import Data.Ord (comparing)
import Data.List (groupBy,sortBy,foldl1')
import Data.Function (on)
import qualified IntMap as M
import IntMap ((!))
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

type DCaps = M.IntMap [(Int,Double)]
type TalkBalance = M.IntMap Int

emptyTalk :: TalkBalance
emptyTalk = M.empty

data UserStats =
  UserStats
    { socUS  :: {-# UNPACK #-} !Double
    , dayUS  :: {-# UNPACK #-} !Int
    , insUS  :: !TalkBalance
    , outsUS :: !TalkBalance
    , totUS  :: !TalkBalance
    , balUS  :: !TalkBalance
    }

newUserStats :: Double -> Int -> UserStats
newUserStats soc day = UserStats {socUS = soc, dayUS = day,
  insUS = emptyTalk, outsUS = emptyTalk, totUS = emptyTalk, balUS = emptyTalk}

type UStats = M.IntMap UserStats
data SocRun =
  SocRun
    { alphaSR   :: {-# UNPACK #-} !Double
    , betaSR    :: {-# UNPACK #-} !Double
    , gammaSR   :: {-# UNPACK #-} !Double
    , socInitSR :: {-# UNPACK #-} !Double
    , maxDaysSR :: !(Maybe Int)
    }

optSocRun = SocRun 0.1 0.5 0.5 1.0 Nothing

data SGraph =
  SGraph
    { drepsSG  :: !Graph
    , dmentsSG :: !Graph
    , dcapsSG  :: !DCaps
    , ustatsSG :: !UStats
    }

type SCParams = (Double,Double,Double)

paramSC :: SocRun -> SCParams
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
    doDays days =
     case fst $ M.findMin days of
             !f -> case fst $ M.findMax days of
                      !l -> (f, l)

-- elemAt absent from IntMap
--       let (!start, _) = M.elemAt 0 days -- i.e. "any map entry"
--           range@(!f,!l) = foldl' minMax1 (start, start) (M.keys days) in
--           range

-- merge two day-ranges results
-- mergeDayRanges dr1 dr2 = M.unionWith minMax2 dr1 dr2


socRun :: Graph -> Graph -> SocRun -> IO (SGraph,Timings)
socRun dreps dments opts = do
    let
      params  = paramSC opts
      socInit = socInitSR opts
      dcaps   = M.empty
      ustats  = M.empty
      sgraph  = SGraph dreps dments dcaps ustats
      dranges = M.unionWith minMax2 (dayRanges dreps) (dayRanges dments)
      l0 = snd . snd $ M.findMax dranges
      (!lastDay0, !dstarts) = foldl' updt (l0, M.empty) (M.assocs dranges)
      -- Data.Map has insertWith' which we used below, but Data.IntMap doesn't:
      -- updt (!ld, !m) (u,(f,l)) = (max ld l, M.insertWith' (++) f [u] m)
      updt (!ld, !m) (u,(f,l)) = (max ld l, M.insertWith (++) f [u] m)
      -- M.alter (\mus -> case mus of Nothing -> Just [u]; Just us -> Just (u:us)) f m)
      firstDay = fst $ M.findMin dstarts
      !lastDay  = let x' = maybe lastDay0 (\y -> min lastDay0 (firstDay + y - 1)) (maxDaysSR opts)
      			      in
                    trace ((show . M.size $ dranges) ++ " total users, doing days from " ++ (show firstDay) ++ " to " ++ (show x'))
                    x'
                    
      tick :: IO (SGraph,Timings) -> Int -> IO (SGraph,Timings)
      tick st day = do
        (sgraph,ts) <- st
        let
        -- inject the users first appearing in this cycle
          nus       = newUserStats socInit day
          !ustats   = ustatsSG sgraph
          newUsers  = let x = dstarts ! day in
                        trace ("adding " ++ (show . length $ x) ++ " new users on day " ++ (show day))
                        x
          !ustats'  = foldl' insn ustats newUsers
          insn m u  = M.insert u nus m
          !sgraph1  = trace ("now got " ++ show (M.size ustats')) sgraph {ustatsSG = ustats'}
          !sgraph2  = socDay sgraph1 params day
        t <- getTiming $ Just ("day " ++ (show day) ++ " timing: ") -- milliseconds
        -- TODO trace t here
        return (sgraph2,t:ts)
    
    foldl' tick (return (sgraph,[])) [firstDay..lastDay]

-- socDay sgraph params day = undefined

safeDivide :: (Fractional a) => a -> a -> a
safeDivide x 0 = 0 -- or x; interesting differences in soccap
safeDivide x y = x / y

-- fst3 (x,_,_) = x

safeDivide3 (x,y,z) (x',y',z') =
  let
    !a = safeDivide x x'
    !b = safeDivide y y'
    !c = safeDivide z z'
  in (a,b,c)

socDay :: SGraph -> SCParams -> Int -> SGraph
socDay sgraph params day =
  let
    (!alpha, !beta, !gamma) = params
    --SGraph {ustatsSG =ustats, dcapsSG =dcaps} = sgraph
    !ustats = ustatsSG sgraph
    !dcaps  = dcapsSG sgraph
    -- users = M.keys ustats

    -- my bangs, needed?
    tsmap !u _ = socUserDaySum sgraph day u
--     tsmap u _ = case socUserDaySum sgraph day u of
--                  (Nothing,st) -> st `seq` (Nothing,st)
--                  (Just x, st) -> x `seq` st `seq` (Just x, st)
    termsStats = {-# SCC "termsStats" #-} M.mapWithKey tsmap ustats
   -- !termsStats = M.mapWithKey (const . socUserDaySum sgraph day) ustats
    sumTerms   = catMaybes . map fst . M.elems $ termsStats

    -- norms = foldl1' (zipWith (+)) sumTerms
    norms@(!a,!b,!c) = {-# SCC "norms" #-}
      let res@(!a,!b,!c) = foldl1' (\(!x,!y,!z) (!x',!y',!z') -> (x+x',y+y',z+z')) sumTerms
      in trace ("day " ++ (show day) ++ " norms: [" ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c) ++ "]")
         res

    tick (numers,!stats) = {-# SCC "socDay.tick" #-}
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
            !soc -> M.alter addDay user res
              where addDay Nothing   = Just [(day,soc)]
                    addDay (Just ds) = Just ((day,soc):ds)

    in
    sgraph {ustatsSG= ustats', dcapsSG= dcaps'}


-- ddarius checked this is as fast as the handmade below:
-- getUserDay user day = M.lookup user >=> M.lookup day
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

socUserDaySum sgraph day user =
  let
    SGraph {drepsSG =dreps, dmentsSG =dments, ustatsSG =ustats} = sgraph
    !stats = {-# SCC "socUDSum.stats" #-} ustats ! user
    dr_ = getUserDay user day dreps
    dm_ = getUserDay user day dments
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
              M.foldWithKey step 0 dr
              where
                 step !to !num !res = {-# SCC "outStep" #-}
                  let !toBal = M.findWithDefault 0 to bal in
                  if toBal >= 0 then res
                  else
                    let !toSoc = getSocCap ustats to in
                      if toSoc == 0.0 then res
                      else
                        let
                          !toOut = M.findWithDefault 1 to outs
                          !toTot = M.findWithDefault 1 to tot
                          !term = fromIntegral (num * toOut * toBal * toTot) * toSoc
                          in
                          res - term -- equivalent to sum of abs terms


        (!inSumBack,!inSumAll) = {-# SCC "inSumBack" #-}
          case dm_ of
            Nothing -> (0.0,0.0)
            Just dm ->
              M.foldWithKey step (0.0,0.0) dm
              where
                 step !to !num res@(!backSum,!allSum) = {-# SCC "inStep" #-}
                  let
                    !toSoc = getSocCap ustats to in
                    if toSoc == 0.0 then res
                    else
                      let
                        !toIn  = M.findWithDefault 1 to ins
                        !toTot = M.findWithDefault 1 to tot
                        !allTerm  = fromIntegral (num * toIn * toTot) * toSoc
                        !toBal = M.findWithDefault 0 to bal
                        !backTerm = if toBal <= 0 then 0.0 else fromIntegral toBal * allTerm
                        in
                        (backSum + backTerm,allSum + allTerm)


        terms = (outSum, inSumBack, inSumAll)


        addMaps       = M.unionWith (+)
        addsMaps      = M.unionsWith (+)
        negateMap     = M.map negate
        -- this is buggy, as uncovered in OCaml -- missing key in first adds positive from second
        -- in OCaml, replaced by hashMergeWithDef, so the op is supplied the left operand, e.g. 0
        -- subtractMaps = M.unionWith (-)

        -- v0
        -- ins'  = case dr_ of {Just dr -> addMaps ins  dr;  _ -> ins}
        -- outs' = case dm_ of {Just dm -> addMaps outs dm; _ -> outs}

        -- v1
        call_some f m x = case x of Nothing -> m; Just v -> f m v
        ins'  = call_some addMaps ins  dr_
        outs' = call_some addMaps outs dm_

        -- doesn't compile:
        -- mayAddMaps x = call_some (addMaps x)
        -- ddarius
        -- call_some = maybe id
        -- dons
        -- ... fmap (addMaps ins) dr_ ...

        -- v2
        -- ddarius:
        -- case m of Nothing -> n; Just x -> j x <=> maybe n j m
        -- mayAddMaps m x = maybe m (addMaps m) x
        -- ins'  = mayAddMaps ins  dr_
        -- outs' = mayAddMaps outs dm_


        -- ziman: M.unionWith (+) `on` maybe M.empty id
        (tot', bal')  =
          case (dr_, dm_) of
            (Just dr, Nothing) -> (addMaps tot dr, addMaps bal dr)
            (Nothing, Just dm) -> (addMaps tot dm, addMaps bal (negateMap dm))
            (Just dr, Just dm) ->
              let t = addsMaps [tot, dr, dm]
                  b = addsMaps [bal, dr, negateMap dm]
              in
              (t,b)

        !stats' = stats {insUS= ins', outsUS= outs', totUS= tot', balUS= bal'}
        in
        (Just terms, stats')
