
{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module PWRandUndead where


-- Combination of PWR and UNDEAD.
-- 1. Run PWR and UNDEAD separately.
-- 2. Build refined lock dependencies.
-- 3. Run refined cycle check.


import Trace
import Examples

import qualified Data.Map as M
import qualified Data.Set as S

import qualified PWR as P
import qualified Undead as U

-- Map lookup, entry must exist.
getM k m = case (M.lookup k m) of
             Just x -> x
             Nothing -> error "impossible, must exist"

-- Build refined lock dependencies where for each acquired lock we record its dot (= set of events).
-- NOTE: We run undead and pwr separately.
undeadRefined :: [Event] -> [(Thread, S.Set Event, Lock, S.Set Lock)]
undeadRefined trace =
    let acqs = [ e | e <- trace, isAcquire $ op e ]
        dots = P.evtDots $ P.pwr trace
        deps = U.evtDeps $ U.undead trace
        third (_,_,x) = x
    in [ (t,eDots,l,ls)
                |
          e <- acqs,                       -- for all acquire events,
          let (t, l, ls) = getM e deps,    -- find its lock dependency
          let eDots = getM e dots,         -- find its dot,
          not $ S.null ls                  -- discard if lockset is empty.
       ]


instance U.LockDep (Thread, S.Set Event, Lock, S.Set Lock) where
   getThread (t,_,_,_) = t
   getLock (_,_,l,_) = l
   getLS (_,_,_,ls) = ls


checkLD4 deps =
    and $
    map (\(d1,d2) -> not (P.happensBefore d1 d2) && not (P.happensBefore d2 d1)) $
    U.allPairs $ map (\(_,d,_,_) -> d) deps


checkAll deps =
    filter (\deps -> checkLD4 deps) $
    U.cycleCheckAll deps


runAll :: [Event] -> IO ()
runAll trace =
     putStrLn $
     unlines $
     map (\lds -> "\n CYCLE: \n" ++ show lds) $
     checkAll $ undeadRefined trace
