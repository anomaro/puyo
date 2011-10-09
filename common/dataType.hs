-- file: typedata.hs
module Common.DataType
     where

import Common.Time  (Time)

--------------------------------------------------------------------------------
--  ��ʑJ��
--------------------------------------------------------------------------------
data Period = Game GameName -- �Q�[���i�Ղ�Ղ�j
            | Configuration -- �Q�[���̐ݒ�

data GameName   = PuyoPuyo  -- �ʏ�̂Ղ�Ղ�

--------------------------------------------------------------------------------
--  �Q�[��
-------------------------------------------------------------------------------- 
-- �t�B�[���h���W�B( �x���W, �w���W )
type AreaPosition   = (PositionY, PositionX)
type PositionY  = Int
type PositionX  = Int 

-- �F
data Color  = Red 
            | Green
            | Blue 
            | Yellow 
            | Purple 
            | AnyColor
        deriving (Show, Eq, Ord, Enum, Bounded) --            | SkyBlue

-- ��
type NumOfPuyos     = Int   -- �g�݂Ղ�̐�
type NumOfPuyo      = Int   -- �Ղ�̐�
type NumOfUnion     = Int   -- �Ղ�̌����� (�Ղ�̐��Ɠ����H)
type NumOfColors    = Int   -- �F�̐�
type NumOfChain     = Int   -- �A����

--------------------------------------------------------------------------------
--  �Q�[���̏�ԑJ��
--------------------------------------------------------------------------------
-- �Q�[���̏�ԑJ�ڂ�\���f�[�^�B
data GamePhase  = BuildPhase    -- �Ղ搶��
                | ControlPhase  -- �v���C���[�̑���
                | DropPhase     -- �������Ղ���Ō�ɋN����Ղ旎��
                | ErasePhase    -- �Ղ����
                | ErasePhase'
                | FallPhase     -- ������܂Ղ�̗���
                | FallPhase'
                | GameOverPhase -- �Q�[���I�[�o�[
                | AnimationPhase  Time GamePhase  -- �d�����ԂƎ��̏�ԑJ��
        deriving (Show, Eq)

--------------------------------------------------------------------------------
--  �X�R�A
--------------------------------------------------------------------------------
data Score  = Score StaticScore DynamicScore
type StaticScore    = ScoreBaseType     -- ������܂Ղ�Ɋ��Z���ꂽ�X�R�A
type DynamicScore   = ScoreBaseType     -- ������܂Ղ�Ɋ��Z����Ă��Ȃ��X�R�A
type ScoreBaseType  = Int           -- �X�R�A�̐��l�̌^�B ��2147483647
