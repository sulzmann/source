
{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module PWR where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import Trace
import Examples


---------------------------------------
-- Set-based PWR

happensBefore :: S.Set Event -> S.Set Event -> Bool
happensBefore es fs = S.isSubsetOf es fs

-- D = Dots of events = set of events.
-- For each event e, D(e) represents all events (including e) that happen before e.
-- For each thread t, D(t) represents all events that happen before wrt the current thread position.
data PWR_State = PWR_State { threadD :: M.Map Thread (S.Set Event)
                           , threadLS :: M.Map Thread (S.Set Lock)
                           , lwD :: M.Map Var (S.Set Event)
                           , rdSync :: M.Map Var (S.Set Thread)
                           , acqD :: M.Map Lock (S.Set Event)
                           , histD :: M.Map Lock [(S.Set Event, S.Set Event)] }

data R = R { evtDots :: M.Map Event (S.Set Event)
           , evtLS :: M.Map Event (S.Set Lock) }

-- Compute dots for each event and lockset.
pwr :: [Event] -> R
pwr trace = fst $
  runState (foldM (\r -> \e ->
              let addR r e = do
                    s <- get
                    let m = evtDots r
                    let m2 = evtLS r
                    return R { evtDots = insEvt s m e
                             , evtLS = insLS s m2 e }
              in
                    case (op e) of
                       Acquire y -> do acqPWR e y
                                       addR r e
                       Release y -> do relPWR e y
                                       addR r e
                       Read x -> do rdPWR e x
                                    addR r e
                       Write x -> do wrPWR e x
                                     addR r e
                       Fork t -> do forkPWR e t
                                    addR r e
                       Join t -> do joinPWR e t
                                    addR r e
                  ) (R {evtDots = M.empty, evtLS = M.empty}) trace)
           (PWR_State { threadD = M.empty
                      , threadLS = M.empty
                      , lwD = M.empty
                      , rdSync = M.empty
                      , acqD = M.empty
                      , histD = M.empty })
   where
      insEvt s m e = ins e (dots (thread e) (threadD s)) m
      insLS s m e = ins e (lockset (thread e) (threadLS s)) m


-- Lookup dots = set of events.
-- If k entry not present yet, yields empty set.
dots k m = case (M.lookup k m) of
             Just xs -> xs
             Nothing -> S.empty

-- Lookup history.
-- If k entry not present yet, yields [].
hist k m = case (M.lookup k m) of
             Just xs -> xs
             Nothing -> []

-- Lookup lockset.
-- If k entry not present yet, yields empty set.
lockset k m = case (M.lookup k m) of
             Just xs -> xs
             Nothing -> S.empty

-- Lookup thread id of readers that synced with last write.
-- NOTE: For each read there must be an initial write.
--       Hence, the nothing case is impossible.
syncedReaders k m = case (M.lookup k m) of
                       Just xs -> xs
                       Nothing -> error "impossible"

-- Update k by adding xs.
add k xs m = M.insert k (S.union (dots k m) xs) m

ins k xs m = M.insert k xs m

-- ROD rule
-- For all lock variables y in the lockset of the current thread.
-- For all (acq_D, rel_D) in hist(y).
-- If acqD <= thread_D then thread_D = thread_D union rel_D.
syncROD d h =
    foldl
      (\d -> \(acq,rel) ->
             if happensBefore acq d
             then S.union rel d
             else d)
      d
      h

relOrdDep e = do
    s <- get
    let t = thread e
    let d_t = foldl
                (\d -> \y -> syncROD d (hist y (histD s)))
                (dots t (threadD s))
                (lockset t (threadLS s))
    put s {threadD = ins t d_t (threadD s)}

-- Add event e to the dots of its thread.
addEvt e = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.singleton e) (threadD s)}

-- Add y to lockset of e (its thread).
addLS e y = do
  let t = thread e
  s <- get
  let ls = S.union (S.singleton y) (lockset t (threadLS s))
  put s {threadLS = ins t ls (threadLS s)}

-- Delete y from lockset of e (its thread).
deleteLS e y = do
  let t = thread e
  s <- get
  let ls = S.delete y (lockset t (threadLS s))
  put s {threadLS = ins t ls (threadLS s)}

-- D_t = D_t cup { acq(y) }
-- LS_t = LS_t cup { y }
-- apply relOrdDep rule
-- Acq_y = D_t
acqPWR e y = do
    addEvt e
    addLS e y
    relOrdDep e
    s <- get
    let t = thread e
    put s {acqD = ins y (dots t (threadD s)) (acqD s)}

-- D_t = D_t cup { rel(y) };
-- apply relOrdDep rule ??
--    NOTE: Seems unnecessary!
--    ONly apply ROD after a sync
--    Same observation applies to wrPWR and forkPWR.
-- add (Acq_y, D_t) to hist(y)
-- LS_t = LS_t - { y }
relPWR e y = do
    addEvt e
    deleteLS e y
    s <- get
    let t = thread e
    put s {histD = ins y ((dots y (acqD s), dots t (threadD s)) : (hist y (histD s))) (histD s) }

-- D_t = D_t cup { wr(x) };
-- LW_x = D_t
-- RdSync_x = {}
wrPWR e x = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.singleton e) (threadD s)}
  s <- get
  put s { lwD = ins x (dots t (threadD s)) (lwD s)
        , rdSync = M.insert x S.empty (rdSync s) }

-- D_t = D_t cup LW(x) cup { rd(x) }
-- if t in RdSync_x
--  then nothing needs to be done as some prior read in the same thread
--       already applied relOrdDep
--  else RdSync_x = RdSync_x cup { t }
--       relOrdDep
--
-- NOTE: If there's a sequence of reads (in the same thread) synced against the same last write,
--       applying relOrdDep for each read is unnecessary.
--
rdPWR e x = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.union (dots x (lwD s)) (S.singleton e)) (threadD s)}
  s <- get
  let ts = syncedReaders x (rdSync s)
  if S.member t ts
    then return ()
    else do put s {rdSync = M.insert x (S.union (S.singleton t) ts) (rdSync s)}
            relOrdDep e

-- D_t1 = D_t1 cup { fork(t2) };
-- D_t2 = D_t1
forkPWR e t2 = do
  let t1 = thread e
  s <- get
  put s {threadD = add t1 ((S.singleton e)) (threadD s)}
  s <- get
  put s {threadD = add t2 (dots t1 (threadD s)) (threadD s)}

-- D_t1 = D_t1 cup D_t2 cup { join(t2) }
-- apply relOrdDep
joinPWR e t2 = do
  let t1 = thread e
  s <- get
  put s {threadD = add t1 (S.union (dots t2 (threadD s)) (S.singleton e)) (threadD s)}
  relOrdDep e


annotatedWithLockSet trace =  putStrLn $ toMDExtra ("LS", \e -> show $ dots e (evtLS $ pwr trace)) trace


annotatedWithPWR trace = putStrLn $ toMDExtra ("PWR", \e -> show $ dots e (evtDots $ pwr trace)) trace
