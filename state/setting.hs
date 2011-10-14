module State.Setting
( GameState
, get
, get_ColorPattern
, newGameState

, GameStateIndex (..)
, GameStateValue
, initialGameState

, yokokuLv1
, yokokuLv2
, yokokuLv3
, yokokuLv4
, yokokuLv5
, yokokuLv6

, flag_quickTrun
, flag_oturi
) where

-- �Q�[�����̐ݒ蓙�����߂�l
-- �F���E�Ղ�̏����鐔�E������܂Ղ惌�[�g�ȂǂȂǁB�B�B

import qualified Common.Number          as Number
import qualified Common.Color           as Color
import qualified Common.Random          as Random (run)

import qualified Data.Vector.Unboxed    as VCU

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data GameState = GameState GameStateValues      -- Int
                           ColorPattern         -- �F�p�^�[��

type GameStateValues    = VCU.Vector GameStateValue
type GameStateValue     = Int
data GameStateIndex     -- GameState�̒l�̎�ނ�\���B
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
defaultGameState :: GameStateValues
defaultGameState =  VCU.generate numOfGSI $ defaultValue . toEnum

initialGameState    :: GameState
initialGameState    =  GameState defaultGameState 
                                 (randomColorPattern $ limitValue max Color)

-- �e�l�̏����l
defaultValue                :: GameStateIndex -> GameStateValue
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

headGSI = minBound  :: GameStateIndex
lastGSI = maxBound  :: GameStateIndex

--------------------------------------------------------------------------------
--  �ǂݎ��
--------------------------------------------------------------------------------
-- �l�����o���B �E�E�E�l�̎�ށiGameStateIndex�j���w�肵�āAGameState������o���B
get :: GameStateIndex -> GameState          -> GameStateValue
get =  \i             -> \(GameState v _)   -> v VCU.! fromEnum i

-- �F�p�^�[��
get_ColorPattern    :: GameState        -> ColorPattern
get_ColorPattern =  \(GameState _ c)    -> c

--------------------------------------------------------------------------------
--  ��������
--------------------------------------------------------------------------------
-- �l������������
newGameState    :: (GameStateValue -> GameStateValue) -> GameStateIndex 
                -> GameState -> GameState
newGameState f i gs@(GameState gv cp)   = GameState gv' cp
  where
    gv' = gv VCU.// [(i', e')]
    i'  = fromEnum i
    e'  | e'' > limitValue max i    = limitValue min i
        | e'' < limitValue min i    = limitValue max i
        | otherwise                 = e''
    e'' = f $ get i gs

--------------------------------------------------------------------------------
--  �\���Ղ�
--------------------------------------------------------------------------------
yokokuLv1    = 1
yokokuLv2 gs    | fieldSizeX > 1    = yokokuLv1 * fieldSizeX
                | otherwise         = 2
  where fieldSizeX = get FieldSizeX gs
--yokokuLv3 gs    | fieldSizeX > 1    = Field.sizeYyokokuLv3 * yokokuLv2 gs
yokokuLv3 gs    | fieldSizeX > 1    = 5 * yokokuLv2 gs
                | otherwise         = 3
  where fieldSizeX = get FieldSizeX gs
yokokuLv4 gs    | fieldSizeX > 1    = fieldSizeX * yokokuLv3 gs
                | otherwise         = 4
  where fieldSizeX = get FieldSizeX gs
yokokuLv5 gs    | fieldSizeX > 1    = 2 * yokokuLv4 gs
                | otherwise         = 5
  where fieldSizeX = get FieldSizeX gs
yokokuLv6 gs    | fieldSizeX > 1    = 2 * yokokuLv5 gs
                | otherwise         = 6
  where fieldSizeX = get FieldSizeX gs

--------------------------------------------------------------------------------
--  �F�p�^�[��
--------------------------------------------------------------------------------
-- �����_���ȐF�p�^�[�������B
randomColorPattern  :: Number.Colors -> ColorPattern
randomColorPattern  =  \noc          -> do
    val     <- Random.run $ Color.totalAssortments noc - 1
    return $ Color.assortments !! val

--------------------------------------------------------------------------------
--  �Q�[����{�ݒ�
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- �N�C�b�N�^�[���̗L��
flag_oturi          = True  :: Bool         -- ������܂Ղ�̂���̗L��