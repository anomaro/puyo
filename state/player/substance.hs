module State.Player.Substance
    where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import Common.Time  (Time)
import Common.Score (Score)
import Common.Color (Color)
import qualified Common.Field           as Field
import qualified Common.Yokoku          as Yokoku
import qualified Common.Phase           as Phase (Game)

--------------------------------------------------------------------------------
--  �v���C���[���
--------------------------------------------------------------------------------
-- �P�l�̃v���C���[���ێ������Ԃ��ЂƂ܂Ƃ߂ɂ����f�[�^�B
-- �i�t�B�[���h��ԁE����Ղ�E���_�Ȃǁj
data PlayerState = PlayerState  Identity.PlayerIdentity
                                GamePhaseState      -- �Q�[����ԑJ��
                                FieldState          -- �t�B�[���h���
                                PlayerPuyoState     -- ����Ղ�̏��
                                NextPuyoState       -- �l�N�X�g�Ղ�ȍ~�̐F
                                ScoreState          -- ���_
                                YokokuState         -- �\���Ղ�i���L�j
                                LoseFlagState       -- �s�k�t���O�i���L�j

type GamePhaseState  = IORF.IORef Phase.Game
type FieldState      = AIO.IOArray Field.Position Area.Area
type PlayerPuyoState = IORF.IORef PlayerPuyo
data PlayerPuyo
    = NonExistent
    | PlayerPuyoInfo    (Color, Color)      -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                        Field.Position      -- ��_�Ղ�̃t�B�[���h���W
                        Direction.Area      -- ���_�Ղ�̕���
                        Time                -- ���R�����p�̃J�E���^
                        Time                -- ��]�p�̃J�E���^
                        Bool                -- �N�C�b�N�^�[���t���O
        deriving (Show, Eq)
        
type NextPuyoState   = IORF.IORef [Color]   
          
type ScoreState      = IORF.IORef Score 

type YokokuState    = IORF.IORef YokokuField
type YokokuField    = (Yokoku.Shelf, Yokoku.Shelf)

type LoseFlagState  = IORF.IORef LoseFlag
type LoseFlag       = (Bool, Bool)

--------------------------------------------------------------------------------
--  ��Ԃ��蔲��
--------------------------------------------------------------------------------
-- �v���C���[��Ԃ������̏�Ԃ����蔲���B
takeout_playerIdentity  :: PlayerState -> Identity.PlayerIdentity
takeout_playerIdentity  (PlayerState pI _ _ _ _ _ _ _)  =  pI

takeout_gamePhaseState  :: PlayerState -> IO GamePhaseState
takeout_gamePhaseState  (PlayerState _ gs _ _ _ _ _ _)  =  return gs

takeout_fieldstate      :: PlayerState -> IO FieldState
takeout_fieldstate      (PlayerState _ _ fs _ _ _ _ _)  =  return fs

takeout_ppuyostate      :: PlayerState -> IO PlayerPuyoState
takeout_ppuyostate      (PlayerState _ _ _ ps _ _ _ _)  =  return ps

takeout_nextPuyoState   :: PlayerState -> IO NextPuyoState
takeout_nextPuyoState   (PlayerState _ _ _ _ ns _ _ _)  =  return ns

takeout_scoreState      :: PlayerState -> ScoreState
takeout_scoreState      (PlayerState _ _ _ _ _ sc _ _)  =  sc

takeout_yokokuState     :: PlayerState -> YokokuState
takeout_yokokuState     (PlayerState _ _ _ _ _ _ ys _)  =  ys

takeout_loseFlagState   :: PlayerState -> LoseFlagState
takeout_loseFlagState   (PlayerState _ _ _ _ _ _ _ ls)  =  ls