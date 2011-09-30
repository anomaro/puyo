-- fallPhase.hs
module Process.Phase.Fall(
    moveYokokuPuyo,
    putOjamaPuyo,
    )
    where

import Data.List (delete)
import Control.Monad

import qualified Common.DataType  as T
import qualified Common.Function    as U
import qualified State.Setting   as V
import qualified Common.Name      as W

import State.Player.DataType    as P
import State.Player.Query (
    get_playerIdentity,
    get_fallOjamaPuyo,
    is_neighborSpace,
    )
import State.Player.Overwriting (
    renew_fieldArea,
    renewYokoku,
    toSupplyYokoku,
    toAdvanceYokoku,
    )

--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
-- 相手側のおじゃまぷよをReserveからSupplyへ、(連鎖によるおじゃまぷよ量の確定。)
-- 自分のおじゃまぷよをSupplyからAdvanceへ（おじゃまぷよ落下の確定。）
moveYokokuPuyo  :: V.GameState -> P.PlayerState -> IO()
moveYokokuPuyo gs state =  do
    toSupplyYokoku other state
    toAdvanceYokoku yokokuLv3 own state
  where 
    own     = fst $ get_playerIdentity state
    other   = U.againstTerritory $ fst $ get_playerIdentity state
    -- 隕石ぷよがもたらすおじゃまぷよの量。
    yokokuLv3 = V.yokokuLv3 gs
    
--------------------------------------------------------------------------------
--  おじゃまぷよを実際に落下させる。
--------------------------------------------------------------------------------
putOjamaPuyo  :: V.GameState -> P.PlayerState -> IO Bool
putOjamaPuyo gs state =  do
    numOfOjama  <- get_fallOjamaPuyo trt state
    renewYokoku ((+) (negate $ sieve numOfOjama)) id id trt state
    
    randomList  <- 2 `fromRandomTo` (fieldSizeX + 1)
    
    mapM_ put $ take numOfOjama $ targetAreaPosition randomList
    
    return $ numOfOjama /= 0
    
  where
    trt         = fst $ get_playerIdentity state
    fieldSizeX  = V.get V.FieldSizeX gs
    sieve n | n > fieldSizeX    = fieldSizeX
            | otherwise         = n
    targetAreaPosition ns = [(V.hidingFieldRank, x)| x <- ns]
    defaultOjamaPuyo    = T.Ojama T.NotYet T.Normal
    -- おじゃまぷよのセッティング
    put p   = do
        isSpace <- is_neighborSpace p T.DPoint state
        if isSpace
          then renew_fieldArea state p defaultOjamaPuyo
          else when W.flag_oturi $ renewYokoku id (+1) id trt state
            

-- nからmまでのランダムなリストを得る。
fromRandomTo :: Int -> Int -> IO [Int]
fromRandomTo n m
    | n == m    = return [n]
    | otherwise = randomList [n..m]

-- ランダムなリストを得る。
randomList      :: [Int] -> IO [Int]
randomList [] =  return []
randomList ns =  do
    ran <- U.runRandom $ length ns - 1
    let n' = ns !! ran
    ns' <- randomList $ delete n' ns
    return $ n' : ns'