-- world.hs
module Common.Name
    where

import qualified Graphics.UI.GLUT   as GLUT (GLsizei)
import qualified Common.DataType   as T

--------------------------------------------------------------------------------
--  �萔
--------------------------------------------------------------------------------
--  �E�B���h�E�T�C�Y 
window_sizeX    =  800  :: GLUT.GLsizei  {- 600, 800 -}
window_sizeY    =  600  :: GLUT.GLsizei  {- 450, 600 -}

--------------------------------------------------------------------------------
--  �Q�[����{�ݒ�
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- �N�C�b�N�^�[���̗L��
flag_oturi          = True  :: Bool         -- ������܂Ղ�̂���̗L��