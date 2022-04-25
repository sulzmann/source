
module Examples where

import Trace

-- NOTE:
-- There's always a main thread.
-- All other threads are created via "fork".


ex1 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Var "x"
  in [ forkE t0 t1,
                    wrE t1 x,      -- w1
       wrE t0 x,                   -- w2
                    rdE t1 x       -- r3
     ]


ex2 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Var "x"
  in [
      wrE t0 x,
      forkE t0 t1,
                    wrE t1 x,
       joinE t0 t1,
       rdE t0 x
     ]

ex3 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Lock "y"
  in [
      acqE t0 x,
      forkE t0 t1,
                    acqE t1 y,
                    relE t1 y,
       joinE t0 t1,
       relE t0 x
     ]

ex4 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      z = Var "z"
  in [
      forkE t0 t1,
      acqE t0 x,
      wrE t0 z,
      relE t0 x,
                    acqE t1 x,
                    rdE t1 z,
                    relE t1 x
     ]


-- Optimization examples.
-- There's no need to apply ROD exhaustively.

-- PWR paper says:
--
--   For events in thread i, we can
--   skip w3 if w3 has been called for some earlier event in thread i and
--   no new critical sections from some other thread are added to the
--   history.

-- The above seems wrong.
-- Check out ex5 and ex6.
--
-- TODO: What has actually been implemented!?

ex5 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Var "y"
      acq t = acqE t x
      rel t = relE t x
      wr t = wrE t y
  in [
      forkE t0 t1,
      acq t0,
      wr t0,
      rel t0,
                    acq t1,    -- Apply ROD
                    wr t1,     -- Skip ROD, okay here
                    rel t1
     ]

ex6 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Var "y"
      acq t = acqE t x
      rel t = relE t x
      wr t = wrE t y
      rd t = rdE t y
  in [
      forkE t0 t1,
      acq t0,
      wr t0,
      rel t0,
                    acq t1,    -- Apply ROD (no sync took place).
                    rd t1,     -- Skip ROD, not okay here! Due to the WRD we have that rel t0 < rd t1.
                    rel t1
     ]


-- ROD Optimization Claim:
-- (1) Skip ROD if the event leaves the dot set of the thread it is in unchanged.
--     Applies to fork, release and write.
--     The exception being acquire.
-- (2) Always apply ROD for acquire.
--      We acquire a new lock, so must check lock history.
-- (3) Skip ROD for subsequent reads from the same thread that are synced against the same write.
--       What if there's an acquire in between?
--       Seems fine cause the acquire applies ROD. See ex7.

-- NOTE: Subsequent acquire? Can also skip.
-- We can ignore a critical section for same thread t
-- if ROD on that critical section has been applied in thread t.


ex7 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Var "y"
      acq t = acqE t x
      rel t = relE t x
      wr t = wrE t y
      rd t = rdE t y
  in [
      acq t0,
      wr t0,
      rel t0,
                    rd t1,
                    acq t1,    -- Apply ROD
                    rd t1,     -- Skip ROD, okay here!
                    rel t1
     ]

-- Example with consecutive reads within the same thread.
-- Only need to apply ROD for the first read (per thread).
ex8 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      forkE t0 t2,
      acqE t0 y,
      wrE t0 x,
      relE t0 y,
      rdE t0 x,
      rdE t0 x,
                 acqE t1 y,
                 rdE t1 x,
                 rdE t1 x,
                 relE t1 y,
                            acqE t2 y,
                            rdE t2 x,
                            rdE t2 x,
                            relE t2 y
      ]


-- Optimization examples.
-- The naive PWR algorithm maintains a global list of the history of all critical sections.
-- Issue:
--   (1) Can only remove a critical seciton if all threads have applied ROD for this critical section.
--   (2) Like WCP, we can maintain thread-local histories.
--
--  NOTE: WCP uses queues. WCP deques as long as a sync must be done.
--  Examples ex9 and ex9b show that for PWR we can not use the same form of iteration through the history.
--
-- ex9:
--  Thread t2 encounters the history [CS_0, CS_1], recording critical sections in the order
--  as they appear in the trace. We apply ROD on CS_1 but not on CS_0.
--
--
-- ex9b:
--  Thread t2 encounters the history [CS_0, CS_1], recording critical sections in the order
--  as they appear in the trace. We apply ROD on CS_0 but not on CS_1.
--
-- ex9 and ex9b show that CS can be removed in that thread after application of ROD.
-- We still might want to catch "consecutive reads", avoids unnecessary scan through
-- the thread-local history.


ex9 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      forkE t0 t2,
      -- CS_0
      acqE t0 y,
      relE t0 y,
                 -- CS_1
                 acqE t1 y,
                 wrE t1 x,
                 relE t1 y,
                            acqE t2 y,
                            rdE t2 x,
                            relE t2 y
      ]


ex9b =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      forkE t0 t2,
      -- CS_0
      acqE t0 y,
      wrE t0 x,
      relE t0 y,
                 -- CS_1
                 acqE t1 y,
                 relE t1 y,
                            acqE t2 y,
                            rdE t2 x,
                            relE t2 y
      ]

-- Update of thread-local histories.
-- Each release adds the critical section to all other threads.
-- Issue:
--   (1) It seems that we need to know all threads beforehand.
--   (2) Otherwise, we need to temporarily store critical sections somewhere.
--   (3) Maybe children simply copy the thread-local history of their parent?



-- CS_1 needs to be added to history of thread t2. And also to t1.
-- CS_0 does not need to be added at all.
-- CS_2 needs to be added to t0 but does not need to be added to t2.
-- Observation: Add critical sections only to active threads.
ex10 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
      z = Var "z"
  in [
      -- CS_0
      acqE t0 y,
      wrE t0 z,
      relE t0 y,

      forkE t0 t1,

      -- CS_1
      acqE t0 y,
      wrE t0 x,
      relE t0 y,
                 -- CS_2
                 acqE t1 y,
                 relE t1 y,
                 wrE t1 x,
                 forkE t0 t2,
                            -- CS_3
                            acqE t2 y,
                            rdE t2 x,
                            rdE t2 z,
                            relE t2 y
      ]



-- Note. Can we avoid adding certain types of critical sections altogether?
-- CS with only reads!? Need to be added at all?

-- Seems no point adding CS_0 and CS_1 to any of the thread-local histories.
-- From the view point of a critical section CS, we only need to "see" other critical sections CS',
-- if events in CS synchronize with events in CS'.
-- Reads only synchronize with writes but not the other way around.
-- Hence, we can ignore critical sections that only consist of reads.
ex11 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      forkE t0 t2,
      wrE t0 x,
      -- CS_0
      acqE t0 y,
      rdE t0 x,
      relE t0 y,
                 -- CS_1
                 acqE t1 y,
                 rdE t1 x,
                 relE t1 y,
                            -- CS_2
                            acqE t2 y,
                            wrE t2 x,
                            relE t2 y
      ]


-- Note.
-- After a thread syncs with a critical section, this critical section can be removed,
-- from the thread-local history.
--
-- For example, CS_0 will be added to thread t1. We apply the ROD rule and can then remove CS_0
-- from t2's thread-local history.
-- Side note. The removal strategy in Kai's thesis won't immediately remove CS_0,
-- only next time once thread t2 applies the ROD rule.
ex12 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      wrE t0 x,
      -- CS_0
      acqE t0 y,
      wrE t0 x,
      relE t0 y,
                 -- CS_1
                 acqE t1 y,
                 rdE t1 x,
                 relE t1 y
      ]




-- CS_0 and CS_2 will be added to thread t2.
-- We find that
--      CS_0.acqE t0 y < rdE t2 x
-- and  CS_2.acqE t1 y < rdE t2 x
-- Hence, we sync twice and remove CS_0 and CS_2.
-- We conclude that
--      CS_0.relE to y < rdE t2 x   (1)
-- and  CS_2.relE t1 y < rdE t2 x
--
-- Point to note.
-- CS_0 added to thread t1.
-- Hence, we conclude that
--      CS_0.relE t0 y < rdE t1 x
-- We also find that
--   rdE t1 x < wrE t1 x < rdE t2 x
--
-- Hence, we could derive (1) by just adding CS_2 to thread t2
-- (thus dropping CS_0).

-- Notation. We write CS_W to denote that the critical section CS only contains writes on variables W.
-- CHECK:
--   Claim: A later CS_W makes an earlier CS'_W obsolete.
--   This ought to hold. Neither CS_W nor CS'_W will sync with any other critical section.
--   If in the current thread we sync against CS_W, then this is due to a WRD.
--   But due to the "last write" assumption, this makes CS'_W obsolete!
ex13 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Var "x"
      y = Lock "y"
  in [
      forkE t0 t1,
      forkE t0 t2,
      wrE t0 x,
      -- CS_0
      acqE t0 y,
      wrE t0 x,
      relE t0 y,
                 -- CS_1
                 acqE t1 y,
                 rdE t1 x,
                 relE t1 y,
                 -- CS_2
                 acqE t1 y,
                 wrE t1 x,
                 relE t1 y,
                            -- CS_3
                            acqE t2 y,
                            rdE t2 x,
                            relE t2 y
      ]


---------------------------------------
-- Examples, Metzger Bachelor

example3 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Lock "y"
      z = Var "z"
  in [
      forkE t0 t1,
      acqE t0 y,
      acqE t0 x,
      wrE t0 z,
      relE t0 x,
      relE t0 y,
                    acqE t1 x,
                    rdE t1 z,
                    acqE t1 y,
                    relE t1 y,
                    relE t1 x
     ]


-- Use WRD for sync.
-- Not exactly the same as read only syncs with write and not vice versa.
example4 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Lock "y"
      z = Lock "z"
      s = Var "s"
  in [
      forkE t0 t1,
      acqE t0 y,
      wrE t0 s,
      acqE t0 z,
      acqE t0 x,
      relE t0 x,
      relE t0 y,
                    rdE t1 s,
                    acqE t1 x,
                    acqE t1 y,
                    relE t1 y,
                    relE t1 x
     ]


example5 =
  let t0 = mainThread
      t1 = nextThread t0
      x = Lock "x"
      y = Lock "y"
      z = Lock "z"
      s = Var "s"
  in [
      forkE t0 t1,
      acqE t0 y,
      wrE t0 s,
      acqE t0 z,
      acqE t0 x,
      relE t0 x,
      relE t0 z,
      relE t0 y,
                    acqE t1 z,
                    rdE t1 s,
                    acqE t1 x,
                    acqE t1 y,
                    relE t1 y,
                    relE t1 x,
                    relE t1 z
     ]



-- False positive.
-- There's an implicit gate lock z due to WRDs.
example6 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      t3 = nextThread t2
      x = Lock "x"
      y = Lock "y"
      z = Lock "z"
      x1 = Var "x1"
      l1 = Lock "l1"
      x2 = Var "x2"
      l2 = Lock "l2"
      x3 = Var "x3"
      l3 = Lock "l3"
      x4 = Var "x4"
      l4 = Lock "l4"
      wr t l x = [acqE t l, wrE t x, relE t l]
      rd t l x = [acqE t l, rdE t x, relE t l]
      aacqrrel t x y = [acqE t x, acqE t y, relE t y, relE t x]
  in [  forkE t0 t1,
        forkE t0 t2,
        forkE t0 t3,
        acqE t0 z]
     ++ wr t0 l1 x1
     ++               rd t1 l1 x1
     ++               aacqrrel t1 x y
     ++               wr t1 l2 x2
     ++ rd t0 l2 x2
     ++ [relE t0 z]
     ++                                                 [acqE t3 z]
     ++                                                 wr t3 l3 x3
     ++                                 rd t2 l3 x3
     ++                                 aacqrrel t2 y x
     ++                                 wr t2 l4 x4
     ++                                                 rd t3 l4 x4
     ++                                                 [relE t3 z]



-- False negative.
example7 =
  let t0 = mainThread
      t1 = nextThread t0
      t2 = nextThread t1
      x = Lock "x"
      y = Lock "y"
  in [
       forkE t0 t1,
       forkE t0 t2,
       acqE t0 x,
                       acqE t1 y,
                       relE t1 y,
       joinE t0 t1,
       relE t0 x,
                                    acqE t2 y,
                                    acqE t2 x,
                                    relE t2 x,
                                    relE t2 y
     ]
