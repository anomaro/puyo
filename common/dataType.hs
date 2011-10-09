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