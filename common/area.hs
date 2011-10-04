module Common.Area
where

import Common.DataType
import Common.Name

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
-- �G���A�i�t�B�[���h�̂P�}�X���������ĂԂ��Ƃɂ���j���\������I�u�W�F�N�g�̎�ށB
data Area   
    = Space
    | Puyo  { color :: Color, union :: UnionCheck, anime :: AnimationType }
    | Ojama {                 union :: UnionCheck, anime :: AnimationType }
    | Wall
        deriving (Show, Eq)

-- �Ղ�̌����`�F�b�N��ԁB
data UnionCheck = NotYet        -- ������
                | Completion    -- ��������
                | EraseFlag     -- ���Ńt���O
        deriving (Show, Eq)
        
-- �t�B�[���h�Ղ�̃A�j���[�V�����̏�Ԃ�\���f�[�^�B
data AnimationType  = Normal                -- �A�j���[�V��������
                    | Dropping   Time       -- ������
                    | Landing    Time Power -- ���n��
                    | Erasing    Time       -- ���Ŏ�
--                    | Projecting Time       -- ���ŗ\����
        deriving (Show, Eq)

type Power  = Double        -- �A�j���[�V�����̋����B

--------------------------------------------------------------------------------
--  ���O
--------------------------------------------------------------------------------
initialFst  = Wall :: Area
initialSnd  = Space :: Area

animeStartDropping  =  Dropping amimeTime_drop    :: AnimationType
animeStartErasing   =  Erasing  amimeTime_erase   :: AnimationType

animeStartLanding   :: Power  -> AnimationType
animeStartLanding p =  Landing amimeTime_land p

defauletPower   = 3 :: Power

defaultOjamaPuyo    =  defaultState Ojama   :: Area

--------------------------------------------------------------------------------
--  �֐�
--------------------------------------------------------------------------------
-- �I�u�W�F�N�g����
isPuyo                  :: Area -> Bool
isPuyo (Puyo  _ _ _)    =  True
isPuyo (Ojama   _ _)    =  True
isPuyo _                =  False

is_colorPuyo                :: Area -> Bool
is_colorPuyo (Puyo _ _ _)   =  True
is_colorPuyo _              =  False

isSpace         :: Area -> Bool
isSpace Space   =  True
isSpace _       =  False

-- �A�j���[�V�������Ԃ��J�E���g
countAT                 :: Area -> Area
countAT (Puyo c uc at)  =  Puyo c uc $ count at
countAT (Ojama  uc at)  =  Ojama  uc $ count at
countAT a               =  a

count                           :: AnimationType -> AnimationType
count ( Dropping n   )  | n > 0 =  Dropping $ n - 1
count ( Landing  n p )  | n > 0 =  Landing  ( n - 1 ) p
count ( Erasing  n   )  | n > 0 =  Erasing  $ n - 1
count _                         =  Normal

-- �f�t�H���g�K�p
defaultState    :: (UnionCheck -> AnimationType -> Area) -> Area
defaultState    =  ($ Normal) . ($ NotYet)
