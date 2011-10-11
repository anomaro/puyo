-- world.hs
module Common.Name
    where

import qualified Graphics.UI.GLUT   as GLUT (GLsizei)
import qualified Common.DataType   as T

--------------------------------------------------------------------------------
--  定数
--------------------------------------------------------------------------------
--  ウィンドウサイズ 
window_sizeX    =  800  :: GLUT.GLsizei  {- 600, 800 -}
window_sizeY    =  600  :: GLUT.GLsizei  {- 450, 600 -}

--------------------------------------------------------------------------------
--  ゲーム基本設定
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- クイックターンの有無
flag_oturi          = True  :: Bool         -- おじゃまぷよのおつりの有無