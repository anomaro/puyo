module Data.Random
( run
, list
) where

import qualified System.Random          as RAN
import qualified Control.Monad.State    as CMS

--------------------------------------------------------------------------------
--  —”
--------------------------------------------------------------------------------
type RandomState a = CMS.State RAN.StdGen a -- StdGen -> (a, StdGen)

-- 0`n‚Ü‚Å‚Ì”ÍˆÍ‚Ì—”‚ð“¾‚éB
get     :: Int -> RandomState Int
get n   =  do
    gen <- CMS.get
    let (val, gen') = RAN.randomR (0,n) gen
    CMS.put gen'
    return val

run     :: Int -> IO Int
run n   =  do
    oldState <- RAN.getStdGen
    let (result, newState) = CMS.runState (get n) oldState
    RAN.setStdGen newState
    return result

list    :: Int -> Int ->  [Int]
list n  =  RAN.randomRs (0, n) . RAN.mkStdGen