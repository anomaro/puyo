-- utility.hs
module Common.Function
    where

import Control.Applicative
import qualified System.Random          as RAN
import qualified Control.Monad.State    as CMS

import qualified Common.DataType       as T
import qualified Common.Direction       as Direction

--------------------------------------------------------------------------------
-- 指定した方向に隣接するフィールド座標を得る。（境界チェックをしない）
-- ゲーム環境を知らない関数が、フィールド可視範囲内でこの関数を使用場合に使う。
neighbor_area           :: Direction.Area -> T.AreaPosition -> T.AreaPosition
neighbor_area Direction.Up    (y, x)   = (y - 1, x    )
neighbor_area Direction.Right (y, x)   = (y    , x + 1)
neighbor_area Direction.Down  (y, x)   = (y + 1, x    )
neighbor_area Direction.Left  (y, x)   = (y    , x - 1)

--------------------------------------------------------------------------------
--  乱数
--------------------------------------------------------------------------------
-- 0～nまでの範囲の乱数を得る。
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