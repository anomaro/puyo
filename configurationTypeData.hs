-- configuration.hs
module ConfigurationTypeData
    where

import Data.Maybe (isJust, fromJust)

import qualified Typedata   as T
import qualified Variable   as V
import qualified World      as W
    
--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
type Selection  = Entry     -- �I�𒆂̍���

data Entry  = FallTime      -- ��������
            | ErasePuyo     -- �Ղ�̏����鐔
            | Color         -- �Ղ�̐F�̐�
            | OjamaRate     -- ������܂Ղ惌�[�g
            | MarginTime    -- �}�[�W���^�C��
            | FieldSizeY    -- �t�B�[���h�T�C�Y�i�c�j�i���͈͂̃T�C�Y�j
            | FieldSizeX    -- �t�B�[���h�T�C�Y�i���j�i���͈͂̃T�C�Y�j
            | NextPuyoView  -- �l�N�X�g�Ղ�ȍ~�̕\����
        deriving (Eq, Enum, Bounded)

data ConfiguPhase   = SelectPhase
                    | AnimationPhase T.Time

defaultConfigAnimePhase = AnimationPhase W.inputTimeConfig

--------------------------------------------------------------------------------
--  �ݒ�f�[�^�Q��
--------------------------------------------------------------------------------
-- ���ڂ̗�
listEntry   = enumFrom $ initialEntry   :: [Entry]

initialEntry    = minBound              :: Entry    -- ��ԍŏ��̍���
lastEntry       = maxBound              :: Entry    -- ��ԍŌ�̍���

-- ���ږ�
toEntryName :: Entry -> String
toEntryName FallTime        =  "Fall Time"
toEntryName ErasePuyo       =  "Erases"
toEntryName Color           =  "Colors"
toEntryName OjamaRate       =  "OjamaPuyoRate"
toEntryName MarginTime      =  "MarginTime"
toEntryName FieldSizeY      =  "FieldSize(Height)"
toEntryName FieldSizeX      =  "FieldSize(Width)"
toEntryName NextPuyoView    =  "NextPuyo"

-- �Q�[�����̒l�Ƃ̑Ή�
toGameStateIndex    :: Entry -> Maybe V.GameStateIndex
toGameStateIndex FallTime       =  Just V.FallTime
toGameStateIndex ErasePuyo      =  Just V.ErasePuyo
toGameStateIndex Color          =  Just V.Color
toGameStateIndex OjamaRate      =  Just V.OjamaRate
toGameStateIndex MarginTime     =  Just V.MarginTime
toGameStateIndex FieldSizeY     =  Just V.FieldSizeY
toGameStateIndex FieldSizeX     =  Just V.FieldSizeX
toGameStateIndex NextPuyoView   =  Just V.NextPuyoView
--toGameStateIndex _              =  Nothing

-- �Q�[�����̒l�𓾂�B
getGameStateValue           :: Entry -> V.GameState -> String
getGameStateValue ent
  | isJust index    = show . V.get (fromJust index) 
  | otherwise       = const "" 
  where
    index   = toGameStateIndex ent
    
-- Selection����
succSelection   :: Selection -> Selection
succSelection sl
    | sl == lastEntry       = minBound
    | otherwise             = succ sl

predSelection   :: Selection -> Selection
predSelection sl
    | sl == initialEntry    = maxBound
    | otherwise             = pred sl
