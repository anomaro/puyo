module Process.Phase.Fall
( moveYokokuPuyo
, putOjamaPuyo
) where

import Data.List (delete)
import Control.Monad

import qualified Common.PlayerIdentity  as Identity (against, territory)
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import qualified Common.Field           as Field
import qualified Common.Random          as Random (run)
import qualified Common.Yokoku          as Yokoku (fixed, fall, exhale, reset)

import qualified State.Setting   as V

import State.Player.DataType    as P
import State.Player.Query (
    get_playerIdentity,
    get_fallOjamaPuyo,
    get_fieldStateArea,
    )
import State.Player.Overwriting (
    renew_fieldArea,
    renewYokoku,
    )

--------------------------------------------------------------------------------
--  
--------------------------------------------------------------------------------
-- ���葤�̂�����܂Ղ��Reserve����Supply�ցA(�A���ɂ�邨����܂Ղ�ʂ̊m��B)
-- �����̂�����܂Ղ��Supply����Advance�ցi������܂Ղ旎���̊m��B�j
moveYokokuPuyo  :: V.GameState -> P.PlayerState -> IO()
moveYokokuPuyo gs state =  do
    renewYokoku Yokoku.fixed other state
    renewYokoku (Yokoku.fall yokokuLv3) own state
  where 
    own     = Identity.territory $ get_playerIdentity state
    other   = Identity.against own
    -- 覐΂Ղ悪�����炷������܂Ղ�̗ʁB
    yokokuLv3 = V.yokokuLv3 gs
    
--------------------------------------------------------------------------------
--  ������܂Ղ�����ۂɗ���������B
--------------------------------------------------------------------------------
putOjamaPuyo  :: V.GameState -> P.PlayerState -> IO Bool
putOjamaPuyo gs state =  do
    numOfOjama  <- get_fallOjamaPuyo trt state
    renewYokoku (Yokoku.exhale (sieve numOfOjama)) trt state
    
    randomList  <- 2 `fromRandomTo` (fieldSizeX + 1)
    
    mapM_ put $ take numOfOjama $ targetAreaPosition randomList
    
    return $ numOfOjama /= 0
    
  where
    trt         = Identity.territory $ get_playerIdentity state
    fieldSizeX  = V.get V.FieldSizeX gs
    sieve n | n > fieldSizeX    = fieldSizeX
            | otherwise         = n
    targetAreaPosition ns = [(Field.hidingBottomRank, x)| x <- ns]
    -- ������܂Ղ�̃Z�b�e�B���O
    put p   = do
        area <- get_fieldStateArea p state
        if Area.isSpace area
          then renew_fieldArea state p Area.defaultOjamaPuyo
          else when V.flag_oturi $ renewYokoku Yokoku.reset trt state

-- n����m�܂ł̃����_���ȃ��X�g�𓾂�B
fromRandomTo :: Int -> Int -> IO [Int]
fromRandomTo n m
    | n == m    = return [n]
    | otherwise = randomList [n..m]

-- �����_���ȃ��X�g�𓾂�B
randomList      :: [Int] -> IO [Int]
randomList [] =  return []
randomList ns =  do
    ran <- Random.run $ length ns - 1
    let n' = ns !! ran
    ns' <- randomList $ delete n' ns
    return $ n' : ns'