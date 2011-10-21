-- configuration.hs
module Data.Configuration
where

import Data.Maybe (isJust, fromJust)

import qualified Data.Setting      as Setting (Item (..), Setting, get)
import qualified Data.Time        as Time (Time, inputConfig)

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
                    | AnimationPhase Time.Time

defaultConfigAnimePhase = AnimationPhase Time.inputConfig

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
toGameStateIndex    :: Entry -> Maybe Setting.Item
toGameStateIndex FallTime       =  Just Setting.FallTime
toGameStateIndex ErasePuyo      =  Just Setting.ErasePuyo
toGameStateIndex Color          =  Just Setting.Color
toGameStateIndex OjamaRate      =  Just Setting.OjamaRate
toGameStateIndex MarginTime     =  Just Setting.MarginTime
toGameStateIndex FieldSizeY     =  Just Setting.FieldSizeY
toGameStateIndex FieldSizeX     =  Just Setting.FieldSizeX
toGameStateIndex NextPuyoView   =  Just Setting.NextPuyoView
--toGameStateIndex _              =  Nothing

-- �Q�[�����̒l�𓾂�B
getGameStateValue           :: Entry -> Setting.Setting -> String
getGameStateValue ent
  | isJust index    = show . Setting.get (fromJust index) 
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