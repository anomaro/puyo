module State.Setting
( Setting
, Item (..)
, Value

, get
, getColorPattern
, renew
, initial

, flag_quickTrun
, flag_oturi
) where

-- �Q�[�����̐ݒ蓙�����߂�l
-- �F���E�Ղ�̏����鐔�E������܂Ղ惌�[�g�ȂǂȂǁB�B�B
import qualified Data.Vector.Unboxed    as VCU

import qualified Common.Number          as Number
import qualified Common.Color           as Color
import qualified Common.Random          as Random (run)

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data Setting   = Setting Values ColorPattern

type Values    = VCU.Vector Value
type Value     = Int
data Item     -- GameState�̒l�̎�ނ�\���B
        = FallTime      -- ��������
        | ErasePuyo     -- �Ղ�̏����鐔
        | Color         -- �Ղ�̐F�̐�
        | OjamaRate     -- ������܂Ղ惌�[�g
        | MarginTime    -- �}�[�W���^�C��
        | FieldSizeY    -- �t�B�[���h�T�C�Y�i�c�j�i���͈͂̃T�C�Y�j
        | FieldSizeX    -- �t�B�[���h�T�C�Y�i���j�i���͈͂̃T�C�Y�j
        | NextPuyoView  -- �l�N�X�g�Ղ�ȍ~�̕\����
        deriving (Show, Eq, Ord, Enum, Bounded)
        
type ColorPattern   = IO Color.ColorAssortment

--------------------------------------------------------------------------------
--  ����l
--------------------------------------------------------------------------------
-- �e�l�̏����l
defaultValue                :: Item -> Value
defaultValue FallTime       =  40
defaultValue ErasePuyo      =  4
defaultValue Color          =  4
defaultValue OjamaRate      =  70
defaultValue MarginTime     =  96  -- ���b�P�ʂ̐��l
defaultValue FieldSizeY     =  12
defaultValue FieldSizeX     =  6
defaultValue NextPuyoView   =  1

-- ���E��
limitValue f FallTime       = f 1   100
limitValue f ErasePuyo      = f 1   100
limitValue f Color          = f 1   Color.maxNumber
limitValue f OjamaRate      = f 1   100
limitValue f MarginTime     = f 1   300
limitValue f FieldSizeY     = f 1   48
limitValue f FieldSizeX     = f 1   24
limitValue f NextPuyoView   = f 0   6

-- GameState�̒l�̎�ށiGameStateIndex�j�̒l�̑����B
numOfGSI    = 1 + fromEnum lastGSI  :: Int

headGSI = minBound  :: Item
lastGSI = maxBound  :: Item

--------------------------------------------------------------------------------
--  �����l
--------------------------------------------------------------------------------
initial :: Setting
initial =  Setting values (randomColorPattern $ limitValue max Color)
  where
    values   =  VCU.generate numOfGSI $ defaultValue . toEnum :: Values

    randomColorPattern      :: Number.Colors -> ColorPattern
    randomColorPattern  noc =  do
        val     <- Random.run $ Color.totalAssortments noc - 1
        return $ Color.assortments !! val

--------------------------------------------------------------------------------
--  �ǂݎ��
--------------------------------------------------------------------------------
-- �l�����o���B �E�E�E�l�̎�ށiGameStateIndex�j���w�肵�āAGameState������o���B
get ::  Item -> Setting          -> Value
get =  \i             -> \(Setting v _)   -> v VCU.! fromEnum i

-- �F�p�^�[��
getColorPattern    :: Setting -> ColorPattern
getColorPattern =  \(Setting _ c)    -> c

--------------------------------------------------------------------------------
--  ��������
--------------------------------------------------------------------------------
-- �l������������
renew    :: (Value -> Value) -> Item -> Setting -> Setting
renew f i gs@(Setting gv cp)   = Setting gv' cp
  where
    gv' = gv VCU.// [(i', e')]
    i'  = fromEnum i
    e'  | e'' > limitValue max i    = limitValue min i
        | e'' < limitValue min i    = limitValue max i
        | otherwise                 = e''
    e'' = f $ get i gs

--------------------------------------------------------------------------------
--  �Q�[����{�ݒ�
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- �N�C�b�N�^�[���̗L��
flag_oturi          = True  :: Bool         -- ������܂Ղ�̂���̗L��