-- file: render.hs
module Render.Game
( render_gameField,       -- ゲームの描画
) where

import Control.Monad
import Control.Applicative

import qualified Graphics.UI.GLUT   as GLUT
import qualified Control.Monad      as MND

import qualified State.Result   as D
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

import qualified Common.PlayerIdentity  as Identity
import qualified Common.Area            as Area

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V
import qualified Common.Name      as W (
    animeTime_rotate,
    amimeTime_drop,
    amimeTime_land,
    sizeYyokokuLv3,
    )
import Render.Object

--------------------------------------------------------------------------------
--  ゲームの描画
--------------------------------------------------------------------------------
render_gameField :: V.GameState -> PlayerState -> D.GameDataCollection -> IO()
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
checkExistent state a =  get_PlayerPuyoExistent state >>= \b  -> MND.when b a
--------------------------------------------------------------------------------
--  基本描画設定
--------------------------------------------------------------------------------
-- 実際の１エリアあたりの大きさ。
unitAreaY   = (*) unitAreaY' . reviseViewSize   :: V.GameState -> Double
unitAreaX   = (*) unitAreaX' . reviseViewSize   :: V.GameState -> Double

-- フィールドサイズ補正
reviseViewSize      :: V.GameState -> Double
reviseViewSize gs   =  min ratioY ratioX
  where
    ratioY  = defaultViewFieldSizeY / fromIntegral (V.fieldSizeY' gs)
    ratioX  = defaultViewFieldSizeX / fromIntegral (V.fieldSizeX' gs)

-- 表示フィールドサイズ既定値。
defaultViewFieldSizeY   = 16    :: Double
defaultViewFieldSizeX   = 8     :: Double

-- フィールドを描画するウィンドウ上の位置。左上のエリアがこの位置に描画される。
field_pointY    :: V.GameState -> Double
field_pointY    =  (+) 1  . unitAreaY

field_pointX :: Identity.Territory -> V.GameState -> Double
field_pointX trt gs = Identity.pick trt (l, r)
  where
    l = - 1 + unitAreaX gs
    r = 1 - unitAreaX gs - (unitAreaX gs * fromIntegral (V.fieldSizeX' gs - 1)) * 2

--(unitAreaX gs * (x' - 1) ) * 2

--------------------------------------------------------------------------------
--  勝利数描画   
--------------------------------------------------------------------------------
render_wins :: V.GameState -> PlayerState -> D.GameDataCollection -> IO()
render_wins gs state gdc    =  do
    GLUT.lineWidth GLUT.$= 2.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objWins
  where
    trt = Identity.territory $ get_playerIdentity state
    xm  = field_pointX trt gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ V.fieldSizeY' gs)
    scale       = 0.001
    objWins     = GLUT.renderString GLUT.Roman $ show wins
      where
        wins    = Identity.pick trt $ D.getWins gdc

--------------------------------------------------------------------------------
--  予告ぷよ描画    
--------------------------------------------------------------------------------
render_yokoku           :: V.GameState -> PlayerState -> IO()
render_yokoku gs state  =  do
    numOfOjama  <- get_yokoku trt state
    zipWithM_ f targetArea $ objs numOfOjama
  where
    targetArea  = [(V.hidingFieldRank, x + 1) | x <- targetPosX]
    targetPosX  = [1..fieldSizeX]
    fieldSizeX  = V.get V.FieldSizeX gs
    objs n      = reverse $ yokokuKinds' n gs
    trt         = Identity.territory $ get_playerIdentity state
    f p o   = render_fieldObject' gs trt p 0 0 1 1 0 o

-- 予告ぷよの表示する種類を決める。
yokokuKinds'    :: T.NumOfPuyo -> V.GameState -> [GameObject]
yokokuKinds' n gs   = yokokuKinds n 0 gs []
  where
    yokokuKinds :: T.NumOfPuyo -> T.PositionX -> V.GameState -> [GameObject]
                -> [GameObject]
    yokokuKinds 0 x gs objs = objs
    yokokuKinds n x gs objs
     | x == fieldSizeX     = objs
     | n >= V.yokokuLv6 gs = yokokuKinds (f V.yokokuLv6) x' gs (g objYokokuLv6)
     | n >= V.yokokuLv5 gs = yokokuKinds (f V.yokokuLv5) x' gs (g objYokokuLv5)
     | n >= V.yokokuLv4 gs = yokokuKinds (f V.yokokuLv4) x' gs (g objYokokuLv4)
     | n >= V.yokokuLv3 gs = yokokuKinds (f V.yokokuLv3) x' gs (g objYokokuLv3)
     | n >= V.yokokuLv2 gs = yokokuKinds (f V.yokokuLv2) x' gs (g objYokokuLv2)
     | otherwise           = yokokuKinds n'              x' gs (g objYokokuLv1)
      where
        fieldSizeX  = V.get V.FieldSizeX gs
        x'  = x + 1
        n'  = n - V.yokokuLv1
        f v = n - v gs
        g o = o : objs

--------------------------------------------------------------------------------
--  得点描画    
--------------------------------------------------------------------------------
render_score            :: V.GameState -> PlayerState -> IO()
render_score gs state   =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    render_gameobject' (GLUT.Vector3 xm ym 0) scale scale 0 objScore
  where
    xm  = field_pointX (Identity.territory $ get_playerIdentity state) gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ V.fieldSizeY' gs)
    scale       = 0.001
    objScore    = do
        score <- get_score state >>= \(T.Score n m ) -> return $ n + m
        GLUT.renderString GLUT.Roman $ show score
    
--------------------------------------------------------------------------------
--  落下予測地点描画
--------------------------------------------------------------------------------
render_fallPoint            :: V.GameState -> PlayerState -> IO()
render_fallPoint gs state   =  do
    (cb, cm)    <- get_PlayerPuyoColors     state
    pos         <- get_PlayerPuyoPosition   state
    d           <- get_PlayerPuyoDirection  state
    
    bottomPosB  <- bottomArea pos
    bottomPosM  <- if d == T.DUp || d == T.DDown
                    then return bottomPosB
                    else bottomArea $ U.neighbor_area d pos
    render_fieldObject gs trt (if d == T.DDown 
                                then U.neighbor_area T.DUp bottomPosB
                                else bottomPosB         
                               ) $ objFallPoint cb
    render_fieldObject gs trt (if d == T.DUp
                                then U.neighbor_area T.DUp bottomPosM
                                else bottomPosM         
                               ) $ objFallPoint cm
    return ()
  where
    -- その列の接地エリア
    bottomArea :: T.AreaPosition -> IO T.AreaPosition
    bottomArea pos    = do
        b   <- is_neighborSpace pos T.DDown state
        if b    then bottomArea $ U.neighbor_area T.DDown pos
                else return pos
    trt = Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
--  ネクストぷよ描画
--------------------------------------------------------------------------------
render_nextPuyo             :: V.GameState -> PlayerState -> IO()
render_nextPuyo gs state    =  do
    colors <- get_nextPuyoColors state
    render_nextPuyo' colors $ V.get V.NextPuyoView gs
  where
    render_nextPuyo' :: [T.Color] -> T.NumOfPuyos -> IO()
    render_nextPuyo' _          0   = return ()
    render_nextPuyo' (cb:cm:cs) n   = do
        let trt = Identity.territory $ get_playerIdentity state
            n'  = (V.get V.NextPuyoView gs - n) * 2
            y   = V.topFieldRank + n'
            x   = if trt == Identity.Left 
                    then V.fieldSizeX' gs + 1
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
render_playerPuyo           :: V.GameState -> PlayerState -> IO()
render_playerPuyo gs state  =  do
    color       <- get_PlayerPuyoColors     state
    (y, x)      <- get_PlayerPuyoPosition   state
    d           <- get_PlayerPuyoDirection  state
    fallTime    <- get_PlayerPuyoFallTime   state
    rotateTime  <- get_PlayerPuyoRotateTime state
    
    render_fieldObject' (y, x) fallTime d rotateTime color
  where
    render_fieldObject' :: T.AreaPosition -> T.Time
                           -> T.Direction -> T.Time -> (T.Color, T.Color)
                           -> IO()
    render_fieldObject' (y, x) fallTime d rotateTime (cb, cm) = do  
        let fallTime'   = V.get V.FallTime gs
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
rotateGap :: V.GameState -> T.Direction -> T.Time -> (Double, Double)
rotateGap gs direction rotateTime  = 
    let r = pi * fromIntegral rotateTime / (2 * fromIntegral W.animeTime_rotate)
    in
    (\(fy, fx) -> (2 * unitAreaY gs * fy r, 2 * unitAreaX gs * fx r) ) 
        (directionalFunctions direction)
      where
        directionalFunctions :: Floating a => T.Direction -> ((a->a), (a->a))
        directionalFunctions T.DUp      = (cos, negate.sin)
        directionalFunctions T.DRight   = (sin, cos)
        directionalFunctions T.DDown    = (negate.cos, sin)
        directionalFunctions T.DLeft    = (negate.sin, negate.cos)
        directionalFunctions _          = (id, id)

--------------------------------------------------------------------------------
--  フィールド背景描画
--------------------------------------------------------------------------------
render_backfield            :: V.GameState -> PlayerState -> IO()
render_backfield gs state   =  MND.mapM_ f $ V.fieldArrayIndices gs
  where
    trt = Identity.territory $ get_playerIdentity state
    f p = render_fieldObject gs trt p objBackfield

--------------------------------------------------------------------------------
--  フィールド描画
--------------------------------------------------------------------------------
-- フィールドを描画
render_field            :: V.GameState -> PlayerState -> IO()
render_field gs state   =  MND.mapM_ f $ V.fieldArrayIndices gs
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
neighborSameColorPuyo           :: Area.Area -> T.AreaPosition -> PlayerState 
                                -> IO[T.Direction]
neighborSameColorPuyo area p state
  | not (Area.isLink area) = return []
  | otherwise              = MND.foldM ffff [] directions
  where
    directions = [T.DRight, T.DDown, T.DLeft, T.DUp]
    ffff    :: [T.Direction] -> T.Direction -> IO[T.Direction]
    ffff acc d  =  do
        area' <- get_fieldStateArea (U.neighbor_area d p) state
        if (Area.isLink area' && Area.color area == Area.color area')
          then return (d : acc)
          else return acc

--------------------------------------------------------------------------------
--  エリア描画    
--------------------------------------------------------------------------------
-- フィールドのぷよの描画。（色ぷよ・おじゃまぷよ）
render_fieldPuyo    :: V.GameState -> Identity.Territory -> Area.AnimationType 
                    -> T.AreaPosition -> GameObject -> IO()
render_fieldPuyo gs trt anime pos@(y, _) obj
    = case Area.morph anime height of
        Nothing -> return ()
        Just ff -> ff (render_fieldObject' gs trt pos) $ obj
      where
        height      = fieldSizeY - fromIntegral y - 1   -- 高さ
        fieldSizeY  = fromIntegral $ V.fieldSizeY' gs 


-- フィールド座標とオブジェクトを指定して、その位置に描画する。
render_fieldObject  :: V.GameState -> Identity.Territory -> T.AreaPosition
                    -> GameObject -> IO()
render_fieldObject gs trt p obj = render_fieldObject' gs trt p 0 0 1 1 0 obj


render_fieldObject' :: V.GameState -> Identity.Territory -> T.AreaPosition ->
                        GLUT.GLdouble -> GLUT.GLdouble ->
                        GLUT.GLdouble -> GLUT.GLdouble ->
                        GLUT.GLdouble -> GameObject -> IO()
render_fieldObject' gs trt (y, x) mx my sx sy r obj = do
    let (y', x') = (fromIntegral y, fromIntegral x)
        posY = field_pointY     gs - (unitAreaY gs * (y' - 1) ) * 2
        posX = field_pointX trt gs + (unitAreaX gs * (x' - 1) ) * 2
        sx'  = sx * reviseViewSize gs
        sy'  = sy * reviseViewSize gs
        my'  = my * unitAreaY gs
    render_gameobject' (GLUT.Vector3 (posX + mx) (posY + my') 0) sx' sy' r obj