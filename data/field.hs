module Data.Field
where

import qualified Data.Direction     as Direction
import qualified Data.Setting       as Setting

--------------------------------------------------------------------------------
--  �^
-------------------------------------------------------------------------------- 
type Position   = (Rank, Line)
type Rank       = Int   -- �x���W
type Line       = Int   -- �w���W

--------------------------------------------------------------------------------
--  ���O
-------------------------------------------------------------------------------- 
-- 覐΂Ղ�̂�����܂Ղ�̗\���i���B
rankInseki  = 5         :: Rank

-- �Ղ�̉��͈͂̍ŏ�i�̃t�B�[���h�x���W�B
topRank     = 4         :: Rank

-- �Ղ�̉��͈͊O�̌����Ȃ��i�̃t�B�[���h�x���W�B
hidingBottomRank = 3         :: Rank

--------------------------------------------------------------------------------
--  �֐�
-------------------------------------------------------------------------------- 
-- �w�肵�������ɗאڂ���t�B�[���h���W�𓾂�B�i���E�`�F�b�N�����Ȃ��j
-- �Q�[������m��Ȃ��֐����A�t�B�[���h���͈͓��ł��̊֐����g�p�ꍇ�Ɏg���B
neighbor :: Direction.Area -> Position -> Position
neighbor Direction.Up    (y, x)   = (y - 1, x    )
neighbor Direction.Right (y, x)   = (y    , x + 1)
neighbor Direction.Down  (y, x)   = (y + 1, x    )
neighbor Direction.Left  (y, x)   = (y    , x - 1)


-- ���͈̓t�B�[���h�T�C�Y �{���̕ǁE�����Ȃ��i�ȏ�̂R�i�B
sizeRank    :: Setting.Setting -> Rank
sizeRank    =  (+) 4 . Setting.get Setting.FieldSizeY

-- ���͈̓t�B�[���h�T�C�Y �{���E�̕�
sizeLine    :: Setting.Setting -> Line
sizeLine    =  (+) 2 . Setting.get Setting.FieldSizeX 


-- �t�B�[���h��Ԃ�ێ������z��̑S�v�f�����X�g�ɂ���B�i�Ղ�̉��͈́j
arrayIndices        :: Setting.Setting -> [Position]
arrayIndices gs     = [(y, x) | y <- [topRank .. (-1 + sizeRank gs)]
                              , x <- [2 ..       (-1 + sizeLine gs)] ]

-- �����_
critical    :: Setting.Setting -> Position
critical gs =  (topRank, (sizeLine gs + 1) `quot` 2)