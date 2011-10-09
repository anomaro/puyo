-- file: render.hs
module Render.Game
( render_gameField,       -- �Q�[���̕`��
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
import qualified Common.Direction       as Direction
import qualified Common.Time            as Time (Time, animeRotate)

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V
import qualified Common.Name      as W (
    sizeYyokokuLv3,
    )
import Render.Object

--------------------------------------------------------------------------------
--  �Q�[���̕`��
--------------------------------------------------------------------------------
render_gameField :: V.GameState -> PlayerState -> D.GameDataCollection -> IO()
render_gameField gs state gdc   =  do
    render_backfield                        gs state     -- �t�B�[���h�w�i�̕`��
    render_field                            gs state     -- �t�B�[���h�̕`��
    checkExistent state $ render_fallPoint  gs state     -- �����\���n�_�̕`��
    checkExistent state $ render_playerPuyo gs state     -- ����Ղ�̕`��
    render_nextPuyo                         gs state     -- Next�Ղ�̕`��
    render_score                            gs state     -- ���_�̕`��
    render_yokoku                           gs state     -- �\���Ղ�̕`��
    render_wins                             gs state gdc -- �������̕`��

-- �v���C���[����Ղ悪���݂��邩�ǂ����`�F�b�N���Ă���A�N�V���������s����B
checkExistent         :: PlayerState -> IO() -> IO()
checkExistent state a =  get_PlayerPuyoExistent state >>= \b  -> MND.when b a
--------------------------------------------------------------------------------
--  ��{�`��ݒ�
--------------------------------------------------------------------------------
-- ���ۂ̂P�G���A������̑傫���B
unitAreaY   = (*) unitAreaY' . reviseViewSize   :: V.GameState -> Double
unitAreaX   = (*) unitAreaX' . reviseViewSize   :: V.GameState -> Double

-- �t�B�[���h�T�C�Y�␳
reviseViewSize      :: V.GameState -> Double
reviseViewSize gs   =  min ratioY ratioX
  where
    ratioY  = defaultViewFieldSizeY / fromIntegral (V.fieldSizeY' gs)
    ratioX  = defaultViewFieldSizeX / fromIntegral (V.fieldSizeX' gs)

-- �\���t�B�[���h�T�C�Y����l�B
defaultViewFieldSizeY   = 16    :: Double
defaultViewFieldSizeX   = 8     :: Double

-- �t�B�[���h��`�悷��E�B���h�E��̈ʒu�B����̃G���A�����̈ʒu�ɕ`�悳���B
field_pointY    :: V.GameState -> Double
field_pointY    =  (+) 1  . unitAreaY

field_pointX :: Identity.Territory -> V.GameState -> Double
field_pointX trt gs = Identity.pick trt (l, r)
  where
    l = - 1 + unitAreaX gs
    r = 1 - unitAreaX gs - (unitAreaX gs * fromIntegral (V.fieldSizeX' gs - 1)) * 2

--(unitAreaX gs * (x' - 1) ) * 2

--------------------------------------------------------------------------------
--  �������`��   
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
--  �\���Ղ�`��    
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

-- �\���Ղ�̕\�������ނ����߂�B
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
--  ���_�`��    
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
--  �����\���n�_�`��
--------------------------------------------------------------------------------
render_fallPoint            :: V.GameState -> PlayerState -> IO()
render_fallPoint gs state   =  do
    (cb, cm)    <- get_PlayerPuyoColors     state
    pos         <- get_PlayerPuyoPosition   state
    d           <- get_PlayerPuyoDirection  state
    
    bottomPosB  <- bottomArea pos
    bottomPosM  <- if d == Direction.Up || d == Direction.Down
                    then return bottomPosB
                    else bottomArea $ U.neighbor_area d pos
    render_fieldObject gs trt (if d == Direction.Down 
                                then U.neighbor_area Direction.Up bottomPosB
                                else bottomPosB         
                               ) $ objFallPoint cb
    render_fieldObject gs trt (if d == Direction.Up
                                then U.neighbor_area Direction.Up bottomPosM
                                else bottomPosM         
                               ) $ objFallPoint cm
    return ()
  where
    -- ���̗�̐ڒn�G���A
    bottomArea :: T.AreaPosition -> IO T.AreaPosition
    bottomArea pos    = do
        b   <- is_neighborSpace pos Direction.Down state
        if b    then bottomArea $ U.neighbor_area Direction.Down pos
                else return pos
    trt = Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
--  �l�N�X�g�Ղ�`��
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
--  ����Ղ�`��
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
    render_fieldObject' :: T.AreaPosition -> Time.Time
                           -> Direction.Area -> Time.Time -> (T.Color, T.Color)
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


-- ��]�̕`��̂���̐��l�B
rotateGap :: V.GameState -> Direction.Area -> Time.Time -> (Double, Double)
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
--  �t�B�[���h�w�i�`��
--------------------------------------------------------------------------------
render_backfield            :: V.GameState -> PlayerState -> IO()
render_backfield gs state   =  MND.mapM_ f $ V.fieldArrayIndices gs
  where
    trt = Identity.territory $ get_playerIdentity state
    f p = render_fieldObject gs trt p objBackfield

--------------------------------------------------------------------------------
--  �t�B�[���h�`��
--------------------------------------------------------------------------------
-- �t�B�[���h��`��
render_field            :: V.GameState -> PlayerState -> IO()
render_field gs state   =  MND.mapM_ f $ V.fieldArrayIndices gs
  where
    f p =  renew_animationType state p         -- �A�j���[�V������Ԃ̍X�V�B 
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

-- �אڂ����G���A�𒲂ׂāA�����F�̂Ղ悪�����������̃��X�g�𓾂�B
neighborSameColorPuyo           :: Area.Area -> T.AreaPosition -> PlayerState 
                                -> IO[Direction.Area]
neighborSameColorPuyo area p state
  | not (Area.isLink area) = return []
  | otherwise              = MND.foldM ffff [] Direction.areas
  where
    ffff    :: [Direction.Area] -> Direction.Area -> IO[Direction.Area]
    ffff acc d  =  do
        area' <- get_fieldStateArea (U.neighbor_area d p) state
        if (Area.isLink area' && Area.color area == Area.color area')
          then return (d : acc)
          else return acc

--------------------------------------------------------------------------------
--  �G���A�`��    
--------------------------------------------------------------------------------
-- �t�B�[���h�̂Ղ�̕`��B�i�F�Ղ�E������܂Ղ�j
render_fieldPuyo    :: V.GameState -> Identity.Territory -> Area.AnimationType 
                    -> T.AreaPosition -> GameObject -> IO()
render_fieldPuyo gs trt anime pos@(y, _) obj
    = case Area.morph anime height of
        Nothing -> return ()
        Just ff -> ff (render_fieldObject' gs trt pos) $ obj
      where
        height      = fieldSizeY - fromIntegral y - 1   -- ����
        fieldSizeY  = fromIntegral $ V.fieldSizeY' gs 


-- �t�B�[���h���W�ƃI�u�W�F�N�g���w�肵�āA���̈ʒu�ɕ`�悷��B
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