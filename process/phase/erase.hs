-- erasePhase.hs
module Process.Phase.Erase
    (
    erase_puyo,
    rewriteSpase_puyo,
    )
    where

import qualified Control.Monad      as MND

import qualified Common.PlayerIdentity  as Identity (against, territory)
import qualified Common.Area            as Area

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V
import qualified Common.Name      as W

import State.Player.DataType
import State.Player.Query
import State.Player.Overwriting

import Standardizable

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- ������Ԃ𒲂ׂĂՂ�����ł�����B�Ղ悪�������ꍇTrue
erase_puyo          :: V.GameState -> PlayerState -> IO Bool
erase_puyo gs state =  do
    renewScoreCalculation (1+) (const []) (const []) state
    MND.foldM f False $ V.fieldArrayIndices gs
  where
    f :: Bool -> T.AreaPosition -> IO Bool
    f b p = do
        area  <- get_fieldStateArea p state 
        numUnion    <- if Area.isUnionCheck Nothing area p
                         then check_union state p
                         else return 0
        bool        <- if numUnion < V.get V.ErasePuyo gs
                         then return False
                         else do
                            area <- get_fieldStateArea p state
                            erase_unionPuyo state p
                            renewScoreCalculation id (numUnion :) (Area.color area :) state 
                            return True
        off_unionCheck state p
        return $ b || bool

-- ������Ԃ𒲂ׂĂՂ�����ł�����B�i���Ńt���O�������Ă���G���A���󔒂ɂ���j
rewriteSpase_puyo           :: V.GameState -> PlayerState -> IO ()
rewriteSpase_puyo gs state  =  do
    MND.mapM_ f $ V.fieldArrayIndices gs
    calculateScoreD state   -- ���_���v�Z���āADynamicScore������������B
    calculateYokoku
  where
    f :: T.AreaPosition -> IO()
    f p = do
        area    <- get_fieldStateArea p state
        MND.when (Area.isReplacedSpace area p)
                 $ renew_fieldArea state p standard
    -- �\���Ղ���Z�o����B
    calculateYokoku =  do
        T.Score ss ds <- get_score state
        let (n, m)  = ds `quotRem` V.get V.OjamaRate gs
        renewYokoku id id (n +) trt state
        renewScore ((ds - m) +) (const m) state
      where
        trt = Identity.against . Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
-- �����`�F�b�N��Ԃ𖢒�����Ԃɂ���B
off_unionCheck :: PlayerState -> T.AreaPosition -> IO()
off_unionCheck state p = do
    area <- get_fieldStateArea p state
    MND.when (Area.isUnionCheckFinished Nothing area p)
        $ renew_fieldArea state p $ Area.modifyUnion (const standard) area

-- �������Ă���Ղ�����ł�����B�i���Ńt���O�𗧂Ă�B�j
erase_unionPuyo :: PlayerState -> T.AreaPosition -> IO()
erase_unionPuyo state p = do
    area <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.eracingPuyo (Just $ Area.color area)
    mapM_ (fff $ Area.color area) listDirection
  where
    fff :: T.Color -> T.Direction -> IO()
    fff color d = do
        area    <- get_fieldStateArea p' state
        MND.when (Area.isEraseOjamaPuyo area)   $ eraseOjamaPuyo  state p
        MND.when (Area.isUnionCheckFinished (Just color) area p') 
                                                $ erase_unionPuyo state p'
      where
        p'  = U.neighbor_area d p
        -- ������܂Ղ�����ł�����B�i���Ńt���O�𗧂Ă�B�j
        eraseOjamaPuyo      :: PlayerState -> T.AreaPosition -> IO()
        eraseOjamaPuyo s p  =  renew_fieldArea s p $ Area.eracingPuyo Nothing


-- �G���A�Ώۂ̌����`�F�b�N�B���̃G���A���`�F�b�N�Ώۂ��ǂ����͎��O�ɒ��ׂĂ���B
check_union :: PlayerState -> T.AreaPosition -> IO T.NumOfUnion
check_union state p    = do
    area <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.unionCheckCompletion area
    MND.foldM (fff $ Area.color area) 1 listDirection
  where
    fff :: T.Color -> T.NumOfUnion -> T.Direction -> IO T.NumOfUnion
    fff color n d = do
        area    <- get_fieldStateArea p' state
        if Area.isUnionCheck (Just color) area p'
          then check_union state p' >>= return . (+ n)
          else return n
      where
        p'  = U.neighbor_area d p

-- �����̃��X�g
listDirection   = [T.DUp, T.DRight, T.DDown, T.DLeft]