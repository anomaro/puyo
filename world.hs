-- world.hs
module World
    where

import qualified Graphics.UI.GLUT   as GLUT (GLsizei)
import qualified Typedata   as T

--------------------------------------------------------------------------------
--  定数
--------------------------------------------------------------------------------
-- フレームレート。この値が約１秒を表す。
frame_rate      = 60  * speed :: T.Time   -- (60)

--  ウィンドウサイズ 
window_sizeX    =  800  :: GLUT.GLsizei  {- 600, 800 -}
window_sizeY    =  600  :: GLUT.GLsizei  {- 450, 600 -}

-- 速度
speed               = 1

-- 入力硬直時間。
inputTime_moveX     = 6   * speed   :: T.Time   -- ぷよを横移動させたとき。
inputTimeConfig     = 3   * speed   :: T.Time   -- 設定画面でカーソル移動時。

-- アニメーション硬直時間。
amimeTime_move      = 1   * speed   :: T.Time   -- 操作ぷよを移動させたとき。
animeTime_rotate    = 5   * speed   :: T.Time   -- 操作ぷよを回転したとき。
amimeTime_land      = 20  * speed   :: T.Time   -- 操作ぷよを着地させたとき。
amimeTime_drop      = 2   * speed   :: T.Time   -- ちぎり落下のとき。
amimeTime_erase     = 40  * speed   :: T.Time   -- ぷよ消滅のとき。

-- 標準のユーザ
defaultUser1P   = (T.TerritoryLeft , T.Com T.Pechipechi)    :: T.PlayerIdentity
defaultUser2P   = (T.TerritoryRight, T.User)                :: T.PlayerIdentity

-- 隕石ぷよのおじゃまぷよの予告段数。
sizeYyokokuLv3  = 5         :: T.PositionY

--------------------------------------------------------------------------------
--  ゲーム基本設定
--------------------------------------------------------------------------------
flag_quickTrun      = False :: Bool         -- クイックターンの有無
flag_oturi          = True  :: Bool         -- おじゃまぷよのおつりの有無

--------------------------------------------------------------------------------
--  アニメーションタイプ
--------------------------------------------------------------------------------
animeStartDropping  =  T.Dropping amimeTime_drop    :: T.AnimationType
animeStartErasing   =  T.Erasing  amimeTime_erase   :: T.AnimationType

animeStartLanding   :: T.Power  -> T.AnimationType
animeStartLanding p =  T.Landing amimeTime_land p

landingDefauletPower    = 3 :: T.Power
