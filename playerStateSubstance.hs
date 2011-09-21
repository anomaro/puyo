module PlayerStateSubstance
    where

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Typedata   as T
import qualified Utility    as U

--------------------------------------------------------------------------------
--  �v���C���[���
--------------------------------------------------------------------------------
-- �P�l�̃v���C���[���ێ������Ԃ��ЂƂ܂Ƃ߂ɂ����f�[�^�B
-- �i�t�B�[���h��ԁE����Ղ�E���_�Ȃǁj
data PlayerState = PlayerState  T.PlayerIdentity
                                GamePhaseState      -- �Q�[����ԑJ��
                                FieldState          -- �t�B�[���h���
                                PlayerPuyoState     -- ����Ղ�̏��
                                NextPuyoState       -- �l�N�X�g�Ղ�ȍ~�̐F
                                ScoreState          -- ���_
                                YokokuState         -- �\���Ղ�i���L�j
                                LoseFlagState       -- �s�k�t���O�i���L�j

type GamePhaseState  = IORF.IORef T.GamePhase 
type FieldState      = AIO.IOArray T.AreaPosition T.Area 
type PlayerPuyoState = IORF.IORef PlayerPuyo  
data PlayerPuyo
    = NonExistent
    | PlayerPuyoInfo    (T.Color,T.Color)   -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                        T.AreaPosition      -- ��_�Ղ�̃t�B�[���h���W
                        T.Direction         -- ���_�Ղ�̕���
                        T.Time              -- ���R�����p�̃J�E���^
                        T.Time              -- ��]�p�̃J�E���^
                        Bool                -- �N�C�b�N�^�[���t���O
        deriving (Show, Eq)
        
type NextPuyoState   = IORF.IORef [T.Color]   
          
type ScoreState      = IORF.IORef (T.Score, ScoreCalculation)    
type ScoreCalculation   = ( T.NumOfChain,   -- �A����
                            [T.NumOfUnion], -- �A����
                            [T.Color]       -- �������Ղ�̐F
                            )
defaultScoreCalculation = (0, [], [])   :: ScoreCalculation

type YokokuState    = IORF.IORef YokokuField
type YokokuField    = (Yokoku, Yokoku)
type Yokoku     = ( YokokuAdvance T.NumOfPuyo,
                    YokokuSupply  T.NumOfPuyo, 
                    YokokuReserve T.NumOfPuyo )
newtype YokokuAdvance a = Advance a
newtype YokokuSupply  a = Supply  a
newtype YokokuReserve a = Reserve a
-- �\��Ղ�𔭐�������A�Ղ�����i�A���j���Ɉ�UReserve�ɒu����A
-- ���̂Ղ����������������AReserve����Supply�Ɉڂ��A
-- ������܂Ղ悪�~�鑤�̃v���C���[�����n����������AAdvance�ɒu�����B
-- Advance�ɒu���ꂽ�ʂ̂�����܂Ղ悪�ŏI�I�ɍ~���Ă���B

instance Functor YokokuAdvance  where
    fmap f (Advance n)  = Advance (f n)
instance Functor YokokuSupply   where
    fmap f (Supply  n)  = Supply  (f n)
instance Functor YokokuReserve  where
    fmap f (Reserve n)  = Reserve (f n)

defaultYokokuField  :: YokokuField
defaultYokokuField  =  ((Advance 0, Supply 0, Reserve 0),
                        (Advance 0, Supply 0, Reserve 0))


type LoseFlagState  = IORF.IORef LoseFlag
type LoseFlag       = (Bool, Bool)

--------------------------------------------------------------------------------
--  ��Ԃ��蔲��
--------------------------------------------------------------------------------
-- �v���C���[��Ԃ������̏�Ԃ����蔲���B
takeout_playerIdentity  :: PlayerState -> T.PlayerIdentity
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