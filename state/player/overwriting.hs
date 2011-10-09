module State.Player.Overwriting
    (   
    -- �Q�[����ԑJ�ڍX�V
    shift_gamePhase,
    
    -- �l�N�X�g�Ղ��ԍX�V
    eat_nextPuyo,
    
    -- �t�B�[���h��ԍX�V
    renew_fieldArea,
    renew_animationType,
    
    -- �z�Ղ��ԍX�V
    renew_playerPuyo,
    renew_playerPuyo',
    remove_playerPuyo,
    
    -- ���_�X�V
    renewScore,

    -- �\���Ղ�X�V
    renewYokoku,
    toSupplyYokoku,
    toAdvanceYokoku,
    
    -- �s�k�t���O�X�V
    renewLoseFlag,
    )
    where

import State.Player.Substance
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import qualified Common.Score           as Score

--------------------------------------------------------------------------------
--  ��ԏ�������
--------------------------------------------------------------------------------
-- �Q�[����Ԃ�ς���B
shift_gamePhase                 :: PlayerState -> T.GamePhase -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase =<< takeout_gamePhaseState state

-- �l�N�X�g�Ղ�������B
eat_nextPuyo    :: PlayerState -> T.NumOfPuyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) =<< takeout_nextPuyoState state
    
--------------------------------------------------------------------------------
--  �s�k�t���O�X�V
--------------------------------------------------------------------------------
-- �s�k�t���O���X�V����B
renewLoseFlag   :: Bool -> Identity.Territory -> PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ Identity.apply trt (const b) loseFlag
  where
    refLoseFlag = takeout_loseFlagState state

--------------------------------------------------------------------------------
--  �\���Ղ�X�V
--------------------------------------------------------------------------------
-- �\���Ղ���X�V����B
renewYokoku :: (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> Identity.Territory -> PlayerState -> IO()
renewYokoku f1 f2 f3 trt state   = do
    yokokuField <- IORF.readIORef $ takeout_yokokuState state
    IORF.writeIORef (takeout_yokokuState state) $ Identity.apply trt f yokokuField
  where
    f (n1, n2, n3) = (fmap f1 n1, fmap f2 n2, fmap f3 n3)

-- �\���Ղ��Reserve����Supply�ֈڂ��B
toSupplyYokoku :: Identity.Territory -> PlayerState -> IO()
toSupplyYokoku trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ Identity.apply trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    shift (n, Supply n2, Reserve n3)    = (n, Supply $ n2 + n3, Reserve 0)

-- �\���Ղ��Supply����Advance�ֈڂ��B
toAdvanceYokoku :: T.NumOfPuyo -> Identity.Territory -> PlayerState -> IO()
toAdvanceYokoku m trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ Identity.apply trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    shift (Advance n1, Supply n2, n)
        | n2 > m    = (Advance $ n1 + m , Supply $ n2 - m, n)
        | otherwise = (Advance $ n1 + n2, Supply 0       , n)

--------------------------------------------------------------------------------
--  ���_�X�V
--------------------------------------------------------------------------------
-- ���_������������B
renewScore  :: (Score.Score -> Score.Score) -> PlayerState -> IO()
renewScore f state  =
    IORF.writeIORef (takeout_scoreState state) . f =<< get_score state

--------------------------------------------------------------------------------
--  �t�B�[���h��ԍX�V
--------------------------------------------------------------------------------
-- �G���A���X�V����B
renew_fieldArea :: PlayerState -> T.AreaPosition -> Area.Area -> IO()
renew_fieldArea state p area    =
    takeout_fieldstate state >>= \stateF -> AIO.writeArray stateF p area

-- �A�j���[�V������Ԃ��X�V����B
renew_animationType :: PlayerState -> T.AreaPosition -> IO()
renew_animationType state p =  do
    stateF  <- takeout_fieldstate state
    area    <- get_fieldStateArea p state
    AIO.writeArray stateF p $ Area.countAT area

--------------------------------------------------------------------------------
--  �z�Ղ��ԍX�V
--------------------------------------------------------------------------------
-- ������������
renew_playerPuyo :: PlayerState 
                    -> Maybe (T.Color, T.Color) -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> Maybe T.AreaPosition     -- ��_�Ղ�̃t�B�[���h���W
                    -> Maybe Direction.Area     -- ���_�Ղ�̕���
                    -> Maybe Time               -- ���R�����p�̃J�E���^
                    -> Maybe Time               -- ��]�p�̃J�E���^
                    -> Maybe Bool               -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    stateP                              <- takeout_ppuyostate state
    PlayerPuyoInfo c' p' d' tf' tt' qf' <- IORF.readIORef stateP
    IORF.writeIORef stateP $ PlayerPuyoInfo (fromMaybe c'  c )
                                            (fromMaybe p'  p )
                                            (fromMaybe d'  d )
                                            (fromMaybe tf' tf)
                                            (fromMaybe tt' tt)
                                            (fromMaybe qf' qf)

-- ���S��������
renew_playerPuyo'   :: PlayerState 
                    -> (T.Color, T.Color)   -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> T.AreaPosition       -- ��_�Ղ�̃t�B�[���h���W
                    -> Direction.Area       -- ���_�Ղ�̕���
                    -> Time                 -- ���R�����p�̃J�E���^
                    -> Time                 -- ��]�p�̃J�E���^
                    -> Bool                 -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
    flip IORF.writeIORef (PlayerPuyoInfo c p d tf tt qf) 
                                        =<< takeout_ppuyostate state

-- ���S��������
remove_playerPuyo :: PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef NonExistent <=< takeout_ppuyostate