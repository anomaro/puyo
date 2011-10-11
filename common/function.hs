-- utility.hs
module Common.Function
    where

import qualified System.Random          as RAN
import qualified Control.Monad.State    as CMS

--------------------------------------------------------------------------------
--  —”
--------------------------------------------------------------------------------
-- 0`n‚Ü‚Å‚Ì”ÍˆÍ‚Ì—”‚ð“¾‚éB
type RandomState a = CMS.State RAN.StdGen a -- StdGen -> (a, StdGen)
getRandom   :: Int -> RandomState Int
getRandom n = do
    gen <- CMS.get
    let (val, gen') = RAN.randomR (0,n) gen
    CMS.put gen'
    return val

runRandom   :: Int -> IO Int
runRandom n =  do
    oldState <- RAN.getStdGen
    let (result, newState) = CMS.runState (getRandom n) oldState
    RAN.setStdGen newState
    return result

makeRandoms :: Int -> Int ->  [Int]
makeRandoms =  \n  -> RAN.randomRs (0,n) . RAN.mkStdGen