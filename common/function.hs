-- utility.hs
module Common.Function
    where

import Control.Applicative
import qualified System.Random          as RAN
import qualified Control.Monad.State    as CMS

import qualified Common.DataType       as T

--------------------------------------------------------------------------------
class Directional d where
    (<-+->) :: d -> d -> d
    negaDirection   :: d -> d
instance Directional T.Direction where
    T.DUp    <-+-> T.DDown  =  T.DPoint
    T.DDown  <-+-> T.DUp    =  T.DPoint
    T.DRight <-+-> T.DLeft  =  T.DPoint
    T.DLeft  <-+-> T.DRight =  T.DPoint
    T.DPoint <-+-> d        =  d
    d        <-+-> _        =  d
    negaDirection T.DUp     =  T.DDown
    negaDirection T.DDown   =  T.DUp
    negaDirection T.DRight  =  T.DLeft
    negaDirection T.DLeft   =  T.DRight
    negaDirection d         =  d
instance Directional T.RotationDirection where
    T.RRight <-+-> T.RLeft  =  T.RPoint
    T.RPoint <-+-> d        =  d
    d        <-+-> T.RPoint =  d
    negaDirection T.RRight  =  T.RLeft
    negaDirection T.RLeft   =  T.RRight
    negaDirection d         =  d

rotate_direction :: T.RotationDirection -> T.Direction -> T.Direction
rotate_direction T.RRight T.DUp     = T.DRight
rotate_direction T.RRight T.DRight  = T.DDown
rotate_direction T.RRight T.DDown   = T.DLeft
rotate_direction T.RRight T.DLeft   = T.DUp
rotate_direction T.RLeft  T.DUp     = T.DLeft
rotate_direction T.RLeft  T.DLeft   = T.DDown
rotate_direction T.RLeft  T.DDown   = T.DRight
rotate_direction T.RLeft  T.DRight  = T.DUp
rotate_direction T.RPoint d         = d

--------------------------------------------------------------------------------
-- 指定した方向に隣接するフィールド座標を得る。（境界チェックをしない）
-- ゲーム環境を知らない関数が、フィールド可視範囲内でこの関数を使用場合に使う。
neighbor_area           :: T.Direction -> T.AreaPosition -> T.AreaPosition
neighbor_area T.DUp    (y, x)   = (y - 1, x    )
neighbor_area T.DRight (y, x)   = (y    , x + 1)
neighbor_area T.DDown  (y, x)   = (y + 1, x    )
neighbor_area T.DLeft  (y, x)   = (y    , x - 1)
neighbor_area _        (y, x)   = (y    , x    )

-- 指定した方向に隣接するフィールド座標を得る。（境界チェックをする）
neighbor_area'   :: T.PositionY -> T.PositionX -> T.Direction -> T.AreaPosition
                    -> T.AreaPosition
neighbor_area' _  _  T.DUp    (y, x) | y > 1    = (y - 1, x    )
neighbor_area' _  x' T.DRight (y, x) | x < x'   = (y    , x + 1)
neighbor_area' y' _  T.DDown  (y, x) | y < y'   = (y + 1, x    )
neighbor_area' _  _  T.DLeft  (y, x) | x > 1    = (y    , x - 1)
neighbor_area' _  _  _        (y, x)            = (y    , x    )


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