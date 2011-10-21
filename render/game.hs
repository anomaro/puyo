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
import qualified Data.Yokoku            as Yokoku (ojamaVolume)
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
    r = 1 - unitAreaX gs - (unitAreaX gs * fromIntegral (Field.sizeLine gs - 1)) * 2

--(unitAreaX gs * (x' - 1) ) * 2

--------------------------------------------------------------------------------
--  勝利数描画   
--------------------------------------------------------------------------------
render_wins :: Setting.Setting -> PlayerState -> Result.Collection -> IO()
render_wins gs state gdc    =  do
    GLUT.lineWidth GLUT.$= 2.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objWins
  where
    trt = Identity.territory $ get_playerIdentity state
    xm  = field_pointX trt gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ Field.sizeRank gs)
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
    zipWithM_ f targetArea $ objs numOfOjama
  where
    targetArea  = [(Field.hidingBottomRank, x + 1) | x <- targetPosX]
    targetPosX  = [1..fieldSizeX]
    fieldSizeX  = Setting.get Setting.FieldSizeX gs
    objs n      = reverse $ yokokuKinds' n gs
    trt         = Identity.territory $ get_playerIdentity state
    f p o   = render_fieldObject' gs trt p 0 0 1 1 0 o

-- 予告ぷよの表示する種類を決める。
yokokuKinds'    :: Number.Puyo -> Setting.Setting -> [GameObject]
yokokuKinds' n gs   = yokokuKinds n 0 gs []
  where
    yokokuKinds :: Number.Puyo -> Field.Line -> Setting.Setting -> [GameObject]
                -> [GameObject]
    yokokuKinds 0 x gs objs = objs
    yokokuKinds n x gs objs
     | x == fieldSizeX = objs
     | n >= volume 6   = yokokuKinds (n - volume 6) x' gs (objYokokuLv6 : objs)
     | n >= volume 5   = yokokuKinds (n - volume 5) x' gs (objYokokuLv5 : objs)
     | n >= volume 4   = yokokuKinds (n - volume 4) x' gs (objYokokuLv4 : objs)
     | n >= volume 3   = yokokuKinds (n - volume 3) x' gs (objYokokuLv3 : objs)
     | n >= volume 2   = yokokuKinds (n - volume 2) x' gs (objYokokuLv2 : objs)
     | otherwise       = yokokuKinds (n - volume 1) x' gs (objYokokuLv1 : objs)
      where
        fieldSizeX  = Setting.get Setting.FieldSizeX gs
        x'  = x + 1
        volume n = Yokoku.ojamaVolume (toEnum $ n + 1) gs

--------------------------------------------------------------------------------
--  得点描画    
--------------------------------------------------------------------------------
render_score            :: Setting.Setting -> PlayerState -> IO()
render_score gs state   =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objScore
  where
    xm  = field_pointX (Identity.territory $ get_playerIdentity state) gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ Field.sizeRank gs)
    scale       = 0.001
    objScore    = do
        score <- get_score state >>= return . Score.display
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
    return ()
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
        let trt = Identity.territory $ get_playerIdentity state
            n'  = (Setting.get Setting.NextPuyoView gs - n) * 2
            y   = Field.topRank + n'
            x   = if trt == Identity.Left 
                    then Field.sizeLine gs + 1
                    else 0
            mx  = if trt == Identity.Left 
                    then unitAreaX gs / 2 * fromIntegral (n'- 2)
                    else negate $ unitAreaX gs / 2 * fromIntegral (n'- 2)
        render_fieldObject' gs trt (y  , x) mx 0 1 1 0 $ objPuyo cm
        render_fieldObject' gs trt (y+1, x) mx 0 1 1 0 $ objPuyo cb
        render_nextPuyo' cs $ n - 1

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
        let fallTime'   = Setting.get Setting.FallTime gs
            (y', x')    = (fromIntegral y, fromIntegral x)
            trt         = Identity.territory $ get_playerIdentity state
            posY    = field_pointY     gs - (unitAreaY gs * (y' - 1) ) * 2
            posX    = field_pointX trt gs + (unitAreaX gs * (x' - 1) ) * 2
            gy  = 2 * unitAreaY gs * ( fromIntegral (fallTime' - fallTime)
                                    / fromIntegral fallTime' - 1)
            (rY, rX) = rotateGap gs d rotateTime
            sc  = reviseViewSize gs
        render_gameobject' (GLUT.Vector3 posX (posY - gy) 0) 
                            sc sc 0 objControlPuyoBack
        render_gameobject' (GLUT.Vector3 (posX + rX) (posY - gy + rY) 0) 
                            sc sc 0 $ objPuyo cm
        render_gameobject' (GLUT.Vector3 posX (posY - gy) 0) 
                            sc sc 0 $ objPuyo cb

-- 回転の描画のずれの数値。
rotateGap :: Setting.Setting -> Direction.Area -> Time.Time -> (Double, Double)
rotateGap gs direction rotateTime  = 
    let r = pi * fromIntegral rotateTime / (2 * fromIntegral Time.animeRotate)
    in
    (\(fy, fx) -> (2 * unitAreaY gs * fy r, 2 * unitAreaX gs * fx r) ) 
        (directionalFunctions direction)
      where
        directionalFunctions :: Floating a => Direction.Area -> ((a->a), (a->a))
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
  | not (Area.isLink area) = return []
  | otherwise              = foldM ffff [] Direction.areas
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
render_fieldObject' gs trt (y, x) mx my sx sy r obj = do
    let (y', x') = (fromIntegral y, fromIntegral x)
        posY = field_pointY     gs - (unitAreaY gs * (y' - 1) ) * 2
        posX = field_pointX trt gs + (unitAreaX gs * (x' - 1) ) * 2
        sx'  = sx * reviseViewSize gs
        sy'  = sy * reviseViewSize gs
        my'  = my * unitAreaY gs
    render_gameobject' (GLUT.Vector3 (posX + mx) (posY + my') 0) sx' sy' r obj