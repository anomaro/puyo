-- file: typedata.hs
module Common.DataType
     where


--------------------------------------------------------------------------------
--  ��ʑJ��
--------------------------------------------------------------------------------
data Period = Game GameName -- �Q�[���i�Ղ�Ղ�j
            | Configuration -- �Q�[���̐ݒ�

data GameName   = PuyoPuyo  -- �ʏ�̂Ղ�Ղ�

--------------------------------------------------------------------------------
--  �Q�[��
--------------------------------------------------------------------------------
-- ����
type Time   = Int
    
-- �t�B�[���h���W�B( �x���W, �w���W )
type AreaPosition   = (PositionY, PositionX)
type PositionY  = Int
type PositionX  = Int 

-- ����
data Direction  = DUp | DRight | DDown | DLeft | DPoint
        deriving (Show, Eq)
-- ��]����
data RotationDirection  = RRight | RLeft | RPoint
        deriving (Show, Eq)
        
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
--  �G���A
--------------------------------------------------------------------------------
-- �G���A�i�t�B�[���h�̂P�}�X���������ĂԂ��Ƃɂ���j���\������I�u�W�F�N�g�̎�ށB
data Area   = Space
             | Puyo Color UnionCheck AnimationType
--            | Puyo Color NumOfUnion UnionCheck AnimationType
            | Ojama UnionCheck AnimationType
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
--  �v���C���[
--------------------------------------------------------------------------------
-- �v���C���[�����ʂ���f�[�^�B
type PlayerIdentity  = (Territory, Player)

-- �\���t�B�[���h�����ʂ���f�[�^�B
data Territory  = TerritoryLeft
                | TerritoryRight
        deriving (Show, Eq)
-- �v���C���[��\���f�[�^�B
data Player     = User          -- ���[�U
                | Com ComName   -- �R���s���[�^�@���O
data ComName    = Pechipechi

--------------------------------------------------------------------------------
--  �X�R�A
--------------------------------------------------------------------------------
data Score  = Score StaticScore DynamicScore
type StaticScore    = ScoreBaseType     -- ������܂Ղ�Ɋ��Z���ꂽ�X�R�A
type DynamicScore   = ScoreBaseType     -- ������܂Ղ�Ɋ��Z����Ă��Ȃ��X�R�A
type ScoreBaseType  = Int           -- �X�R�A�̐��l�̌^�B ��2147483647
