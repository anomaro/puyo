module State.Player.Overwriting
( shift_gamePhase
, eat_nextPuyo
, renew_fieldArea
, renew_animationType
, renew_playerPuyo
, renew_playerPuyo'
, remove_playerPuyo
, renewScore
, renewYokoku
, renewLoseFlag
) where

import qualified State.Player.Substance as P'
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Data.Setting  as V
import qualified Data.PlayerIdentity  as Identity
import qualified Data.Area            as Area
import qualified Data.Direction       as Direction
import Data.Time  (Time)
import qualified Data.Score           as Score
import Data.Color (Color)
import qualified Data.Field           as Field (Position)
import qualified Data.Yokoku          as Yokoku
import qualified Data.Number          as Number
import qualified Data.Phase           as Phase (Game)

--------------------------------------------------------------------------------
--  ��ԏ�������
--------------------------------------------------------------------------------
-- �Q�[����Ԃ�ς���B
shift_gamePhase                 :: P'.PlayerState -> Phase.Game -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase $ P'.phase state

-- �l�N�X�g�Ղ�������B
eat_nextPuyo    :: P'.PlayerState -> Number.Puyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) $ P'.nexts state
    
--------------------------------------------------------------------------------
--  �s�k�t���O�X�V
--------------------------------------------------------------------------------
-- �s�k�t���O���X�V����B
renewLoseFlag   :: Bool -> Identity.Territory -> P'.PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ Identity.apply trt (const b) loseFlag
  where
    refLoseFlag = P'.loseFlag state

--------------------------------------------------------------------------------
--  �\���Ղ�X�V
--------------------------------------------------------------------------------
-- �\���Ղ���X�V����B
renewYokoku :: (Yokoku.Shelf -> Yokoku.Shelf)
            -> Identity.Territory -> P'.PlayerState -> IO()
renewYokoku f trt state   = do
    yokoku <- IORF.readIORef $ P'.yokoku state
    IORF.writeIORef (P'.yokoku state) $ Identity.apply trt f yokoku

--------------------------------------------------------------------------------
--  ���_�X�V
--------------------------------------------------------------------------------
-- ���_������������B
renewScore  :: (Score.Score -> Score.Score) -> P'.PlayerState -> IO()
renewScore f state  =
    IORF.writeIORef (P'.score state) . f =<< get_score state

--------------------------------------------------------------------------------
--  �t�B�[���h��ԍX�V
--------------------------------------------------------------------------------
-- �G���A���X�V����B
renew_fieldArea :: P'.PlayerState -> Field.Position -> Area.Area -> IO()
renew_fieldArea state p area    = AIO.writeArray (P'.field state) p area

-- �A�j���[�V������Ԃ��X�V����B
renew_animationType         :: P'.PlayerState -> Field.Position -> IO()
renew_animationType state p =  get_fieldStateArea p state >>=
                               AIO.writeArray (P'.field state) p . Area.countAT

--------------------------------------------------------------------------------
--  �z�Ղ��ԍX�V
--------------------------------------------------------------------------------
-- ������������
renew_playerPuyo :: P'.PlayerState 
                    -> Maybe (Color, Color)     -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> Maybe Field.Position     -- ��_�Ղ�̃t�B�[���h���W
                    -> Maybe Direction.Area     -- ���_�Ղ�̕���
                    -> Maybe Time               -- ���R�����p�̃J�E���^
                    -> Maybe Time               -- ��]�p�̃J�E���^
                    -> Maybe Bool               -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    P'.PlayerPuyoInfo c' p' d' tf' tt' qf'  <- IORF.readIORef stateP
    IORF.writeIORef stateP $ P'.PlayerPuyoInfo  (fromMaybe c'  c )
                                                (fromMaybe p'  p )
                                                (fromMaybe d'  d )
                                                (fromMaybe tf' tf)
                                                (fromMaybe tt' tt)
                                                (fromMaybe qf' qf)
  where stateP = P'.playerPuyo state

-- ���S��������
renew_playerPuyo'   :: P'.PlayerState 
                    -> (Color, Color)       -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> Field.Position       -- ��_�Ղ�̃t�B�[���h���W
                    -> Direction.Area       -- ���_�Ղ�̕���
                    -> Time                 -- ���R�����p�̃J�E���^
                    -> Time                 -- ��]�p�̃J�E���^
                    -> Bool                 -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
  flip IORF.writeIORef (P'.PlayerPuyoInfo c p d tf tt qf) $ P'.playerPuyo state

-- ���S��������
remove_playerPuyo :: P'.PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef P'.NonExistent . P'.playerPuyo