-- world.hs
module Common.Name
    where

import qualified Graphics.UI.GLUT   as GLUT (GLsizei)
import qualified Common.DataType   as T

--------------------------------------------------------------------------------
--  �萔
--------------------------------------------------------------------------------
-- �t���[�����[�g�B���̒l����P�b��\���B
frame_rate      = 60  * speed :: T.Time   -- (60)

--  �E�B���h�E�T�C�Y 
window_sizeX    =  800  :: GLUT.GLsizei  {- 600, 800 -}
window_sizeY    =  600  :: GLUT.GLsizei  {- 450, 600 -}

-- ���x
speed               = 1

-- ���͍d�����ԁB
inputTime_moveX     = 6   * speed   :: T.Time   -- �Ղ�����ړ��������Ƃ��B
inputTimeConfig     = 3   * speed   :: T.Time   -- �ݒ��ʂŃJ�[�\���ړ����B

-- �A�j���[�V�����d�����ԁB
amimeTime_move      = 1   * speed   :: T.Time   -- ����Ղ���ړ��������Ƃ��B
animeTime_rotate    = 5   * speed   :: T.Time   -- ����Ղ����]�����Ƃ��B
amimeTime_land      = 20  * speed   :: T.Time   -- ����Ղ�𒅒n�������Ƃ��B
amimeTime_drop      = 2   * speed   :: T.Time   -- �����藎���̂Ƃ��B
amimeTime_erase     = 40  * speed   :: T.Time   -- �Ղ���ł̂Ƃ��B

-- 覐΂Ղ�̂�����܂Ղ�̗\���i���B
sizeYyokokuLv3  = 5         :: T.PositionY

--------------------------------------------------------------------------------
--  �Q�[����{�ݒ�
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- �N�C�b�N�^�[���̗L��
flag_oturi          = True  :: Bool         -- ������܂Ղ�̂���̗L��