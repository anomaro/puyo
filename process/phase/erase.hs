-- erasePhase.hs
module Process.Phase.Erase
    (
    erase_puyo,
    rewriteSpase_puyo,
    )
    where

import qualified Control.Monad      as MND

import qualified Common.PlayerIdentity  as Identity (against, territory)

import qualified Common.DataType   as T (
    UnionCheck(..),
    Color(AnyColor),
    Area(..),
    AreaPosition,
    NumOfUnion,
    Direction(..),
    Score(..),
    )
import qualified Common.Function    as U (
    neighbor_area,
    )
import qualified State.Setting  as V
import qualified Common.Name      as W ( animeStartErasing )

import State.Player.DataType
import State.Player.Query
import State.Player.Overwriting

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
        numUnion    <- if isTarget area T.AnyColor p T.NotYet
                         then check_union state p
                         else return 0
        bool        <- if numUnion < V.get V.ErasePuyo gs
                         then return False
                         else do
                            T.Puyo c _ _  <- get_fieldStateArea p state
                            erase_unionPuyo state p
                            renewScoreCalculation id (numUnion:) (c:) state 
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
        MND.when (isTarget area T.AnyColor p T.EraseFlag)
                 $ renew_fieldArea state p T.Space
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
    MND.when (isTarget area T.AnyColor p T.Completion)
        $ rewrite_unionCheck state p T.NotYet

-- �������Ă���Ղ�����ł�����B�i���Ńt���O�𗧂Ă�B�j
erase_unionPuyo :: PlayerState -> T.AreaPosition -> IO()
erase_unionPuyo state p = do
    T.Puyo color _ _  <- get_fieldStateArea p state
    renew_fieldArea state p $ T.Puyo color T.EraseFlag W.animeStartErasing
    mapM_ (fff color) listDirection
  where
    fff :: T.Color -> T.Direction -> IO()
    fff color d = do
        area    <- get_fieldStateArea p' state
        erase_ojamaPuyo area state p'
        MND.when (isTarget area color p' T.Completion)
          $ erase_unionPuyo state p'
      where
        p'  = U.neighbor_area d p

-- ������܂Ղ�����ł�����B�i���Ńt���O�𗧂Ă�B�j
erase_ojamaPuyo :: T.Area -> PlayerState -> T.AreaPosition -> IO()
erase_ojamaPuyo (T.Ojama T.NotYet _) state p    =
    renew_fieldArea state p $ T.Ojama T.EraseFlag W.animeStartErasing
erase_ojamaPuyo _                    _     _    = return ()

-- �G���A�Ώۂ̌����`�F�b�N�B���̃G���A���`�F�b�N�Ώۂ��ǂ����͎��O�ɒ��ׂĂ���B
check_union :: PlayerState -> T.AreaPosition -> IO T.NumOfUnion
check_union state p    = do
    T.Puyo color _ _  <- get_fieldStateArea p state
    rewrite_unionCheck state p T.Completion
    MND.foldM (fff color) 1 listDirection
  where
    fff :: T.Color -> T.NumOfUnion -> T.Direction -> IO T.NumOfUnion
    fff color n d = do
        area    <- get_fieldStateArea p' state
        if isTarget area color p' T.NotYet
          then check_union state p' >>= return . (+ n)
          else return n
      where
        p'  = U.neighbor_area d p

-- �Ώۂ̃G���A�������`�F�b�N�E�Ղ���ł̑Ώۂ��ǂ�������B
isTarget :: T.Area -> T.Color -> T.AreaPosition -> T.UnionCheck -> Bool
isTarget (T.Puyo c uc _)          c'         (y, _) uc'
  | y >= V.topFieldRank && uc == uc'    = c' == T.AnyColor || c == c'
isTarget (T.Ojama  T.EraseFlag _) T.AnyColor (y, _) T.EraseFlag
                                        = y >= V.topFieldRank
isTarget _ _ _ _                        = False


--����������Ԃ̏��������B�i�F�Ղ�̃G���A��ΏۂɎg�p����j
rewrite_unionCheck :: PlayerState -> T.AreaPosition -> T.UnionCheck -> IO()
rewrite_unionCheck state p uc  = do
    T.Puyo color _ at <- get_fieldStateArea p state
    renew_fieldArea state p $ T.Puyo color uc at
    
-- �����̃��X�g
listDirection   = [T.DUp, T.DRight, T.DDown, T.DLeft]