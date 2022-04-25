
{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module HB where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import Trace


---------------------------------------
-- Set-based happens-before

-- For each event we compute the set of events that happen before.
-- We refer to sets of events as "dots".

{-

Toughts.

Set-based notation is just a different representation for the HB relation!
Just the transitive closure.

What's the point of the set representation?
Maybe easier to formulate "consistency" conditions.
Easier to formulate replay?
Build trace reorderings that respect the happens-before relation.

Tricky point. WRDs = write-read dependencies.
Need to guarantee that WRDs are stable.
See NOTE below.

-}


-- D = Dots of events = set of events.
data HB_State = HB_State { threadD :: M.Map Thread (S.Set Event)
                         , lwD :: M.Map Var (S.Set Event)
                         , relD :: M.Map Lock (S.Set Event) }



-- Compute dots for each event.
dotsHB :: [Event] -> M.Map Event (S.Set Event)
dotsHB trace = fst $
  runState (foldM (\m -> \e -> case (op e) of
                                  Acquire y -> do acqHB e y
                                                  s <- get
                                                  return $ ins e (dots (thread e) (threadD s)) m
                                  Release y -> do relHB e y
                                                  s <- get
                                                  return $ ins e (dots (thread e) (threadD s)) m
                                  Read x -> do rdHB e x
                                               s <- get
                                               return $ ins e (dots (thread e) (threadD s)) m
                                  Write x -> do wrHB e x
                                                s <- get
                                                return $ ins e (dots (thread e) (threadD s)) m
                                  Fork t -> do forkHB e t
                                               s <- get
                                               return $ ins e (dots (thread e) (threadD s)) m
                                  Join t -> do joinHB e t
                                               s <- get
                                               return $ ins e (dots (thread e) (threadD s)) m
                  ) M.empty trace)
           (HB_State { threadD = M.empty, lwD = M.empty, relD = M.empty })


-- Lookup dots = set of events.
-- If k entry not present yet, yields empty set.
dots k m = case (M.lookup k m) of
             Just xs -> xs
             Nothing -> S.empty

-- Update k by adding xs.
add k xs m = M.insert k (S.union (dots k m) xs) m

ins k xs m = M.insert k xs m

-- D_t = D_t cup Rel(y) { acq(y) }
acqHB e y = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.union (dots y (relD s)) (S.singleton e)) (threadD s)}

-- D_t = D_t cup { rel(y) };
-- Rel_y = D_t
relHB e y = do
  let t = thread e
  s <- get
  put s {threadD = add t ((S.singleton e)) (threadD s)}
  s <- get
  put s {relD = ins y (dots t (threadD s)) (relD s)}

-- D_t = D_t cup { wr(x) };
-- LW_x = D_t
wrHB e x = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.singleton e) (threadD s)}
  s <- get
  put s {lwD = ins x (dots t (threadD s)) (lwD s)}

-- D_t = D_t cup LW(x) cup { rd(x) }
rdHB e x = do
  let t = thread e
  s <- get
  put s {threadD = add t (S.union (dots x (lwD s)) (S.singleton e)) (threadD s)}

-- D_t1 = D_t1 cup { fork(t2) };
-- D_t2 = D_t1
forkHB e t2 = do
  let t1 = thread e
  s <- get
  put s {threadD = add t1 ((S.singleton e)) (threadD s)}
  s <- get
  put s {threadD = add t2 (dots t1 (threadD s)) (threadD s)}

-- D_t1 = D_t1 cup D_t2 cup { join(t2) }
joinHB e t2 = do
  let t1 = thread e
  s <- get
  put s {threadD = add t1 (S.union (dots t2 (threadD s)) (S.singleton e)) (threadD s)}

-- hapensBefore es fs if es is a strict subset of fs.
-- Points to note:
-- 1. Set are like sets but ordered.
-- 2. There are no duplicates (left-bias).
-- 3. D = Dots of events
-- 4. For each event e, D(e) represent all events (including e) that happen before e.
-- 5. The "last" write w for each read r is the "nearest" write in D(r).
-- NOTE. This condition is a bit brittle and relies on the properties of Haskell Set.
happensBefore :: S.Set Event -> S.Set Event -> Bool
happensBefore es fs = S.isSubsetOf es fs


runHB trace = do
  let hb_pairs = [ (e,f) | e <- trace, f <- trace, e < f, thread e /= thread f ]
  let happensBefore2 e f = happensBefore (dots e $ dotsHB trace) (dots f $ dotsHB trace)
  let out = [ case (happensBefore2 e f, happensBefore2 f e ) of
                (True, False) -> show e ++ " <HB " ++ show f
                (False, True) -> show f ++ " <HB " ++ show e
                (False, False) -> show e ++ " ||HB " ++ show f
                _ -> error "impossible"
             | (e,f) <- hb_pairs ]
  return ()
  putStrLn $ unlines $ [toMD trace,"\n"] ++ out


annotatedWithHB trace = putStrLn $ toMDExtra ("HB", \e -> show $ dots e (dotsHB trace)) trace



----------------------------
-- examples


{-



Challenge:

Compute some information to represent the happens-before relation.
Based on this information we can



-}
