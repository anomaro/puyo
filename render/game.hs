module Render.Game
( render_gameField
) where

import qualified Graphics.UI.GLUT   as GLUT
import Control.Monad
import Control.Applicative

import State.Player.DataType
import State.Player.Query   (
    get_playerIdentity,
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_nextPuyoColors,
    get_fieldStateArea,
    is_neighborSpace,
    get_score,
    get_yokoku,
    )
import State.Player.Overwriting (
    renew_animationType,
    )
import qualified Data.PlayerIdentity    as Identity
import qualified Data.Area              as Area
import qualified Data.Direction         as Direction
import qualified Data.Time              as Time (Time, animeRotate)
import qualified Data.Score             as Score
import Data.Color (Color)
import qualified Data.Field             as Field
import qualified Data.Number            as Number
import qualified Data.Yokoku            as Yokoku (view)
import qualified Data.Setting           as Setting
import qualified Data.Result            as Result (Collection, wins)
import Render.Object

--------------------------------------------------------------------------------
--  ゲームの描画
--------------------------------------------------------------------------------
render_gameField :: Setting.Setting -> PlayerState -> Result.Collection -> IO()
render_gameField gs state gdc   =  do
    render_backfield                        gs state     -- フィールド背景の描画
    render_field                            gs state     -- フィールドの描画
    checkExistent state $ render_fallPoint  gs state     -- 落下予測地点の描画
    checkExistent state $ render_playerPuyo gs state     -- 操作ぷよの描画
    render_nextPuyo                         gs state     -- Nextぷよの描画
    render_score                            gs state     -- 得点の描画
    render_yokoku                           gs state     -- 予告ぷよの描画
    render_wins                             gs state gdc -- 勝利数の描画

-- プレイヤー操作ぷよが存在するかどうかチェックしてからアクションを実行する。
checkExistent         :: PlayerState -> IO() -> IO()
checkExistent state a =  get_PlayerPuyoExistent state >>= \b  -> when b a
--------------------------------------------------------------------------------
--  基本描画設定
--------------------------------------------------------------------------------
-- 実際の１エリアあたりの大きさ。
unitAreaY   = (*) unitAreaY' . reviseViewSize   :: Setting.Setting -> Double
unitAreaX   = (*) unitAreaX' . reviseViewSize   :: Setting.Setting -> Double

-- フィールドサイズ補正
reviseViewSize      :: Setting.Setting -> Double
reviseViewSize gs   =  min ratioY ratioX
  where
    ratioY  = defaultViewFieldSizeY / fromIntegral (Field.sizeRank gs)
    ratioX  = defaultViewFieldSizeX / fromIntegral (Field.sizeLine gs)

-- 表示フィールドサイズ既定値。
defaultViewFieldSizeY   = 16    :: Double
defaultViewFieldSizeX   = 8     :: Double

-- フィールドを描画するウィンドウ上の位置。左上のエリアがこの位置に描画される。
field_pointY    :: Setting.Setting -> Double
field_pointY    =  (+) 1  . unitAreaY

field_pointX :: Identity.Territory -> Setting.Setting -> Double
field_pointX trt gs = Identity.pick trt (l, r)
  where
    l = - 1 + unitAreaX gs
    r = 1 - unitAreaX gs - (renderPositionX gs $ Field.sizeLine gs)

-- フィールド座標を実際の画面の座標位置に変換。
renderPositionX         :: Setting.Setting -> Field.Line -> Double
renderPositionX gs x    =  (unitAreaX gs * fromIntegral (x - 1) ) * 2

renderPositionY         :: Setting.Setting -> Field.Rank -> Double
renderPositionY gs y    =  (unitAreaY gs * fromIntegral (y - 1) ) * 2

--------------------------------------------------------------------------------
--  勝利数描画   
--------------------------------------------------------------------------------
render_wins :: Setting.Setting -> PlayerState -> Result.Collection -> IO()
render_wins gs state gdc    =  do
    GLUT.lineWidth GLUT.$= 2.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objWins
  where
    xm = field_pointX trt gs + unitAreaX gs
    ym = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ Field.sizeRank gs)
    trt = Identity.territory $ get_playerIdentity state
    scale       = 0.001
    objWins     = GLUT.renderString GLUT.Roman $ show wins
      where
        wins    = Identity.pick trt $ Result.wins gdc

--------------------------------------------------------------------------------
--  予告ぷよ描画    
--------------------------------------------------------------------------------
render_yokoku           :: Setting.Setting -> PlayerState -> IO()
render_yokoku gs state  =  do
    numOfOjama  <- get_yokoku trt state
    zipWithM_ f renderPositions (Yokoku.view numOfOjama gs)
  where
    trt         = Identity.territory $ get_playerIdentity state
    f p k   = render_fieldObject' gs trt p 0 0 1 1 0 (yokoku k)
    renderPositions  = [(Field.hidingBottomRank, x + 1) | x <- [1..fieldSizeX]]
    fieldSizeX  = Setting.get Setting.FieldSizeX gs

--------------------------------------------------------------------------------
--  得点描画    
--------------------------------------------------------------------------------
render_score            :: Setting.Setting -> PlayerState -> IO()
render_score gs state   =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objScore
  where
    xm  = field_pointX trt gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * fromIntegral (Field.sizeRank gs)
    trt = Identity.territory $ get_playerIdentity state
    scale       = 0.001
    objScore    = do
        score <- Score.display `fmap` get_score state
        GLUT.renderString GLUT.Roman $ show score
    
--------------------------------------------------------------------------------
--  落下予測地点描画
--------------------------------------------------------------------------------
render_fallPoint            :: Setting.Setting -> PlayerState -> IO()
render_fallPoint gs state   =  do
    (cb, cm)    <- get_PlayerPuyoColors     state
    pos         <- get_PlayerPuyoPosition   state
    d           <- get_PlayerPuyoDirection  state
    bottomPosB  <- bottomArea pos
    bottomPosM  <- if d == Direction.Up || d == Direction.Down
                    then return bottomPosB
                    else bottomArea $ Field.neighbor d pos
    render_fieldObject gs trt (if d == Direction.Down 
                                then Field.neighbor Direction.Up bottomPosB
                                else bottomPosB         
                               ) $ objFallPoint cb
    render_fieldObject gs trt (if d == Direction.Up
                                then Field.neighbor Direction.Up bottomPosM
                                else bottomPosM         
                               ) $ objFallPoint cm
  where
    -- その列の接地エリア
    bottomArea :: Field.Position -> IO Field.Position
    bottomArea pos    = do
        b   <- is_neighborSpace pos Direction.Down state
        if b    then bottomArea $ Field.neighbor Direction.Down pos
                else return pos
    trt = Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
--  ネクストぷよ描画
--------------------------------------------------------------------------------
render_nextPuyo             :: Setting.Setting -> PlayerState -> IO()
render_nextPuyo gs state    =  do
    colors <- get_nextPuyoColors state
    render_nextPuyo' colors $ Setting.get Setting.NextPuyoView gs
  where
    render_nextPuyo' :: [Color] -> Number.PuyoPair -> IO()
    render_nextPuyo' _          0   = return ()
    render_nextPuyo' (cb:cm:cs) n   = do
        render_fieldObject' gs trt (y  , x) mx 0 1 1 0 $ objPuyo cm
        render_fieldObject' gs trt (y+1, x) mx 0 1 1 0 $ objPuyo cb
        render_nextPuyo' cs $ n - 1
      where trt = Identity.territory $ get_playerIdentity state
            n'  = (Setting.get Setting.NextPuyoView gs - n) * 2
            y   = Field.topRank + n'
            x   = if trt == Identity.Left  then Field.sizeLine gs + 1  else 0
            mx  = if trt == Identity.Left 
                    then unitAreaX gs / 2 * fromIntegral (n'- 2)
                    else negate $ unitAreaX gs / 2 * fromIntegral (n'- 2)
        
--------------------------------------------------------------------------------
--  操作ぷよ描画
--------------------------------------------------------------------------------
render_playerPuyo           :: Setting.Setting -> PlayerState -> IO()
render_playerPuyo gs state  =  do
    color       <- get_PlayerPuyoColors     state
    (y, x)      <- get_PlayerPuyoPosition   state
    d           <- get_PlayerPuyoDirection  state
    fallTime    <- get_PlayerPuyoFallTime   state
    rotateTime  <- get_PlayerPuyoRotateTime state
    render_fieldObject' (y, x) fallTime d rotateTime color
  where
    render_fieldObject' :: Field.Position -> Time.Time
                           -> Direction.Area -> Time.Time -> (Color, Color)
                           -> IO()
    render_fieldObject' (y, x) fallTime d rotateTime (cb, cm) = do  
        render_gameobject' (GLUT.Vector3 posX (posY - gy) 0) 
                            sc sc 0 objControlPuyoBack
        render_gameobject' (GLUT.Vector3 (posX + rX) (posY - gy + rY) 0) 
                            sc sc 0 $ objPuyo cm
        render_gameobject' (GLUT.Vector3 posX (posY - gy) 0) 
                            sc sc 0 $ objPuyo cb
      where
        trt         = Identity.territory $ get_playerIdentity state
        posY    = field_pointY     gs - renderPositionY gs y
        posX    = field_pointX trt gs + renderPositionX gs x
        fallTime'   = Setting.get Setting.FallTime gs
        gy  = 2 * unitAreaY gs * ( fromIntegral (fallTime' - fallTime)
                                / fromIntegral fallTime' - 1)
        (rY, rX) = rotateGap gs d rotateTime
        sc  = reviseViewSize gs

-- 回転の描画のずれの数値。
rotateGap :: Setting.Setting -> Direction.Area -> Time.Time -> (Double, Double)
rotateGap gs d rotateTime  = (2 * unitAreaY gs * fy r, 2 * unitAreaX gs * fx r)
  where
    r = pi * fromIntegral rotateTime / (2 * fromIntegral Time.animeRotate)
    (fy, fx) = (directionalFunctions d)
      where
        directionalFunctions Direction.Up      = (cos, negate.sin)
        directionalFunctions Direction.Right   = (sin, cos)
        directionalFunctions Direction.Down    = (negate.cos, sin)
        directionalFunctions Direction.Left    = (negate.sin, negate.cos)

--------------------------------------------------------------------------------
--  フィールド背景描画
--------------------------------------------------------------------------------
render_backfield            :: Setting.Setting -> PlayerState -> IO()
render_backfield gs state   =  mapM_ f $ Field.arrayIndices gs
  where
    trt = Identity.territory $ get_playerIdentity state
    f p = render_fieldObject gs trt p objBackfield

--------------------------------------------------------------------------------
--  フィールド描画
--------------------------------------------------------------------------------
-- フィールドを描画
render_field            :: Setting.Setting -> PlayerState -> IO()
render_field gs state   =  mapM_ f $ Field.arrayIndices gs
 where
  f p =  renew_animationType state p         -- アニメーション状態の更新。 
         >> (matching =<< get_fieldStateArea p state)
   where
    matching a
      | Area.is_colorPuyo a = renderColorPuyoLink a (Area.anime a)
      | Area.isPuyo a   = render_fieldPuyo gs trt (Area.anime a) p objOjamaPuyo
      | otherwise       = return ()
    trt = Identity.territory $ get_playerIdentity state
    renderColorPuyoLink area animeTime = 
        neighborSameColorPuyo area p state
        >>= render_fieldPuyo gs trt animeTime p . objPuyo' (Area.color area)

-- 隣接したエリアを調べて、同じ色のぷよがあった方向のリストを得る。
neighborSameColorPuyo           :: Area.Area -> Field.Position -> PlayerState 
                                -> IO[Direction.Area]
neighborSameColorPuyo area p state
  | Area.isLink area    = foldM ffff [] Direction.areas
  | otherwise           = return []
  where
    ffff    :: [Direction.Area] -> Direction.Area -> IO[Direction.Area]
    ffff acc d  =  do
        area' <- get_fieldStateArea (Field.neighbor d p) state
        if (Area.isLink area' && Area.color area == Area.color area')
          then return (d : acc)
          else return acc

--------------------------------------------------------------------------------
--  エリア描画    
--------------------------------------------------------------------------------
-- フィールドのぷよの描画。（色ぷよ・おじゃまぷよ）
render_fieldPuyo :: Setting.Setting -> Identity.Territory -> Area.AnimationType 
                 -> Field.Position -> GameObject -> IO()
render_fieldPuyo gs trt anime pos@(y, _) obj
    = case Area.morph anime height of
        Nothing -> return ()
        Just ff -> ff (render_fieldObject' gs trt pos) $ obj
      where
        height      = fieldSizeY - fromIntegral y - 1   -- 高さ
        fieldSizeY  = fromIntegral $ Field.sizeRank gs 


-- フィールド座標とオブジェクトを指定して、その位置に描画する。
render_fieldObject  :: Setting.Setting -> Identity.Territory -> Field.Position
                    -> GameObject -> IO()
render_fieldObject gs trt p obj = render_fieldObject' gs trt p 0 0 1 1 0 obj

render_fieldObject' :: Setting.Setting -> Identity.Territory -> Field.Position
                    -> GLUT.GLdouble -> GLUT.GLdouble
                    -> GLUT.GLdouble -> GLUT.GLdouble
                    -> GLUT.GLdouble -> GameObject -> IO()
render_fieldObject' gs trt (y, x) mx my sx sy r obj =
    render_gameobject' (GLUT.Vector3 (posX + mx) (posY + my') 0) sx' sy' r obj
  where posY = field_pointY     gs - renderPositionY gs y
        posX = field_pointX trt gs + renderPositionX gs x
        sx'  = sx * reviseViewSize gs
        sy'  = sy * reviseViewSize gs
        my'  = my * unitAreaY gs