

module Trace where

-- A trace is a sequence of events.
-- Each event is identified via
--    (1) location number
--    (2) thread id
--    (3) kind of operation


import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State


newtype Loc = Loc Int deriving (Eq, Ord)
unLoc (Loc i) = i
newtype Lock = Lock String deriving (Eq, Ord)
newtype Var = Var String deriving (Eq, Ord)
newtype Thread = Thread Int deriving (Eq, Ord)
unThread (Thread i) = i

instance Show Loc where
    show (Loc i) = show i
instance Show Lock where
    show (Lock x) = x
instance Show Var where
    show (Var x) = x
instance Show Thread where
    show (Thread i) = show i

data Op = Read Var
        | Write Var
        | Acquire Lock
        | Release Lock
        | Fork Thread
        | Join Thread deriving (Eq, Ord)

isAcquire (Acquire{}) = True
isAcquire _ = False

instance Show Op where
    show (Read x) = "rd(" ++ show x ++ ")"
    show (Write x) = "wr(" ++ show x ++ ")"
    show (Acquire x) = "acq(" ++ show x ++ ")"
    show (Release x) = "rel(" ++ show x ++ ")"
    show (Fork x) = "fork(" ++ show x ++ ")"
    show (Join x) = "join(" ++ show x ++ ")"

data Event = Event { loc :: Loc,
                     op :: Op,
                     thread :: Thread } deriving (Eq, Ord)

instance Show Event where
    show e = show (op e) ++ "_" ++ show (loc e)

-------------------------------------------------------
-- Interface for specifying traces

dummyLoc = Loc (-1)

-- Dummy loc for now. See "addLoc" below.
rdE t x     = Event {op = Read x, thread = t, loc = dummyLoc }
wrE t x     = Event {op = Write x, thread = t, loc = dummyLoc }
acqE t x    = Event {op = Acquire x, thread = t, loc = dummyLoc }
relE t x    = Event {op = Release x, thread = t, loc = dummyLoc }
forkE t1 t2 = Event {op = Fork t2, thread = t1, loc = dummyLoc }
joinE t1 t2 = Event {op = Join t2, thread = t1, loc = dummyLoc }

-- We start counting with zero.
mainThread = Thread 0
nextThread (Thread i) = Thread (i+1)

-- User must guarantee that traces are well-formed.
exampleTrace =
  let t0 = mainThread
      t1 = nextThread t0
      x = Var "x"
      y = Lock "y"

  in [
   wrE t0 x,
   acqE t0 y,
   rdE t0 x,
   forkE t0 t1,
                     wrE t1 x,
   joinE t0 t1,
   relE t0 y
   ]


-- Apply on each trace to add proper location numbers.
addLoc es = map (\(e,l) -> e { loc = Loc l }) $ zip es [1..]


nTimes n x = take n $ repeat x

-- Tabular display, can be used for markdown.
toMD :: [Event] -> String
toMD = toMDExtra ("", \_ -> "")

-- General version where we can add some extra row.
toMDExtra :: (String, Event -> String) -> [Event] -> String
toMDExtra (lastRow, genLast) es =
  let firstRow = 4
      row = 9
      adj row xs
       | length xs >= row = error "fix row size"
       | otherwise = xs ++ nTimes (row - length xs) ' '
      m = maximum $ map (\e -> unThread $ thread e) es
  in
    foldl (\s -> \e -> let l = show (loc e) ++ "."
                           i = unThread $ thread e
                           r = adj firstRow l ++ nTimes (row * i) ' '
                               ++ adj row (show (op e)) ++ nTimes (row * (m-i)) ' ' ++ genLast e
                       in s ++ "\n" ++ r)
          (concat $ ["   "] ++ [adj row ("T" ++ show i) | i <- [0..m]] ++ [adj row lastRow])
          es
