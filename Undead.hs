
{-# LANGUAGE GADTs, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}

module Undead where

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import Trace
import Examples

import Debug.Trace


---------------------------------------
-- UNDEAD
--
-- Builds lock dependencies and checks for cycles.


data Undead_State =
    Undead_State { threadLS :: M.Map Thread (S.Set Lock)
                      -- lockset
                 , deps :: S.Set (Thread, Lock, S.Set Lock)
                      -- lock deps, only record if lockset is non-empty
                      -- for example, (t, x, {}) will be ignored

                 , evtDeps :: M.Map Event (Thread, Lock, S.Set Lock)
                      -- lock deps associated to acquire
                      -- acq(x) associated to for example (t, x, {})
                 }


undead :: [Event] -> Undead_State
undead trace = snd $
  runState (foldM_ (\_ -> \e ->
             let go act = case (op e) of
                            Acquire y -> do acqU e y
                                            act
                            Release y -> do relU e y
                                            act
                            Read x -> do rdU e x
                                         act
                            Write x -> do wrU e x
                                          act
                            Fork t -> do forkU e t
                                         act
                            Join t -> do joinU e t
                                         act
             in go (return ()))  () trace)
           (Undead_State { threadLS = M.empty
                         , evtDeps = M.empty
                         , deps = S.empty })



ins k xs m = M.insert k xs m

-- Lookup lockset.
-- If k entry not present yet, yields empty set.
lockset k m = case (M.lookup k m) of
             Just xs -> xs
             Nothing -> S.empty


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

-- add (t, y, LS_t)
-- LS_t = LS_t cup { y }
acqU e y = do
    let t = thread e
    s <- get
    let ls = lockset t (threadLS s)
    put s { deps = if S.null ls
                   then deps s
                   else S.insert (t,y,ls) (deps s)
          , evtDeps = M.insert e (t,y,ls) (evtDeps s) }
    addLS e y

-- LS_t = LS_t - { y }
relU e y = do
    deleteLS e y

wrU e x = do
  return ()

rdU e x = do
  return ()

forkU e t2 = do
  return ()

joinU e t2 = do
  return ()



annotatedWithDep trace =
    putStrLn $ toMDExtra ("Dep", \e -> case M.lookup e (evtDeps $ undead trace) of
                                          Just x -> show x
                                          Nothing -> "") trace

-----------------------------------------
-- Cycle checking


-- All permutations for sublists of size n.
-- Assume n <= length of list.
allPermN :: Eq a => Int -> [a] -> [[a]]
allPermN n xs = nub [ take n ys | ys <- permutations xs ]

-- All combination of pairs ignoring the order within a pair.
allPairs :: [a] -> [(a,a)]
allPairs [x,y] = [(x,y)]
allPairs (x:xs) = [(x,y) | y <- xs] ++ (allPairs xs)

class LockDep a where
   getThread :: a -> Thread
   getLock :: a -> Lock
   getLS :: a -> S.Set Lock

instance LockDep (Thread, Lock, S.Set Lock) where
   getThread (t,_,_) = t
   getLock (_,l,_) = l
   getLS (_,_,ls) = ls

-- Check if there's a cycle for a specific order of lock dependencies.
checkLockDepForCycle :: LockDep a => [a] -> Bool
checkLockDepForCycle deps =
  -- Dependencies must be distinct threads.
  -- This check is necessary as we accumulate all dependencies and
  -- then permute them all. Hence, we might encounter dependencies
  -- from the same thread.
  (length (nub threads) == length threads)
  &&
  -- LD-1.
  -- Locksets are pairwise disjoint.
  (and $ map (\(ls1,ls2) -> S.disjoint ls1 ls2) $ allPairs $ map (\ld -> getLS ld) deps)
  &&
  -- LD-2 + LD-3.
  -- Lock acquired in following lockset.
  (acqCycle (head deps) deps)
  where
     threads = map (\ld -> getThread ld) deps

     -- LD-3
     acqCycle ld1 [ld2] = S.member (getLock ld2) (getLS ld1)
     -- LD-2
     acqCycle d (ld1:(ds@(ld2:_))) = S.member (getLock ld1) (getLS ld2) && acqCycle d ds


cycleCheck :: [(Thread, Lock, S.Set Lock)] -> Bool
cycleCheck deps =
 or [ or $ map checkLockDepForCycle (allPermN n deps) | n <- [2..length deps] ]


orr [] = (False, undefined)   -- if no cycle = False, undefined will do.
orr ((True,d):_) = (True,d)
orr ((False,_):ds) = orr ds



cycleCheckFirst :: (LockDep a, Eq a) => [a] -> (Bool, [a])
cycleCheckFirst deps =
 orr [ orr $ map (\ d -> (checkLockDepForCycle d, d)) (allPermN n deps) | n <- [2..length deps] ]




cycleCheckAll :: (LockDep a, Eq a) => [a] -> [[a]]
cycleCheckAll deps =
 concat
 [ map (\(_,d) -> d) $
   filter (\(b,_) -> b) $
   map (\ d -> (checkLockDepForCycle d, d)) (allPermN n deps) | n <- [2..length deps] ]


-- True if there's no lock deps cycle, otherwise False.
run :: [Event] -> Bool
run trace = cycleCheck $ S.toList $ deps $ undead trace

-- Reports first detected lock deps cycle, if any.
runFirst :: [Event] -> IO ()
runFirst trace = case (cycleCheckFirst $ S.toList $ deps $ undead trace) of
                    (False,_) -> putStrLn "No cycle detected"
                    (True,d) -> putStrLn $ show d

runAll :: [Event] -> IO ()
runAll trace =
     putStrLn $
     unlines $
     map (\lds -> "\n CYCLE: \n" ++ show lds) $
     cycleCheckAll $ S.toList $ deps $ undead trace
