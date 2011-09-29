-- file: render.hs
module Render.Game
( render_gameField,       -- �Q�[���̕`��
) where

import Control.Monad
import Control.Applicative

import qualified Graphics.UI.GLUT   as GLUT
import qualified Control.Monad      as MND

import qualified GameDataCollection     as D
import PlayerState
import QueryPS  (
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
import OverwritingPS    (
    renew_animationType,
    )

import qualified Typedata   as T
import qualified Utility    as U
import qualified Variable   as V
import qualified World      as W (
    animeTime_rotate,
    amimeTime_drop,
    amimeTime_land,
    landingDefauletPower,
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

field_pointX                        :: T.Territory -> V.GameState -> Double
field_pointX T.TerritoryLeft  gs    =  - 1 + unitAreaX gs
field_pointX T.TerritoryRight gs    = 
    1 - unitAreaX gs - (unitAreaX gs * fromIntegral (V.fieldSizeX' gs - 1)) * 2

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
    trt = fst $ get_playerIdentity state
    xm  = field_pointX trt gs + unitAreaX gs
    ym  = field_pointY gs - unitAreaY gs * 2 * (fromIntegral $ V.fieldSizeY' gs)
    scale       = 0.001
    objWins     = do
        GLUT.renderString GLUT.Roman $ show wins
      where
        wins    = f trt $ D.getWins gdc
        f T.TerritoryLeft   = fst
        f T.TerritoryRight  = snd

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
    trt         = fst $ get_playerIdentity state
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
    xm  = field_pointX (fst $ get_playerIdentity state) gs + unitAreaX gs
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
    -- ���̗�̐ڒn�G���A
    bottomArea :: T.AreaPosition -> IO T.AreaPosition
    bottomArea pos    = do
        b   <- is_neighborSpace pos T.DDown state
        if b    then bottomArea $ U.neighbor_area T.DDown pos
                else return pos
    trt = fst $ get_playerIdentity state

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
        let trt = fst $ get_playerIdentity state
            n'  = (V.get V.NextPuyoView gs - n) * 2
            y   = V.topFieldRank + n'
            x   = if trt == T.TerritoryLeft 
                    then V.fieldSizeX' gs + 1
                    else 0
            mx  = if trt == T.TerritoryLeft 
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
    render_fieldObject' :: T.AreaPosition -> T.Time
                           -> T.Direction -> T.Time -> (T.Color, T.Color)
                           -> IO()
    render_fieldObject' (y, x) fallTime d rotateTime (cb, cm) = do  
        let fallTime'   = V.get V.FallTime gs
            (y', x')    = (fromIntegral y, fromIntegral x)
            (trt, _)    = get_playerIdentity state
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
--  �t�B�[���h�w�i�`��
--------------------------------------------------------------------------------
render_backfield            :: V.GameState -> PlayerState -> IO()
render_backfield gs state   =  MND.mapM_ f $ V.fieldArrayIndices gs
  where
    trt = fst $ get_playerIdentity state
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
        matching (T.Puyo c T.NotYet T.Normal) = renderColorPuyoLink c T.Normal
        matching (T.Puyo c T.EraseFlag a)     = renderColorPuyoLink c a
        matching (T.Puyo c _ a) = render_fieldPuyo gs trt a p $ objPuyo' c []
        matching (T.Ojama  _ a) = render_fieldPuyo gs trt a p objOjamaPuyo
        matching _              = return ()
        
        trt = fst $ get_playerIdentity state
        renderColorPuyoLink color animeTime = 
            neighborSameColorPuyo p color state
            >>= render_fieldPuyo gs trt animeTime p . objPuyo' color 

-- �אڂ����G���A�𒲂ׂāA�����F�̂Ղ悪�����������̃��X�g�𓾂�B
neighborSameColorPuyo           :: T.AreaPosition -> T.Color -> PlayerState 
                                -> IO[T.Direction]
neighborSameColorPuyo p c state =  do
    right   <- ffff T.DRight
    down    <- ffff T.DDown
    left    <- ffff T.DLeft
    up      <- ffff T.DUp
    return $ right ++ down ++ left ++ up
  where
    directions = [T.DRight, T.DDown, T.DLeft, T.DUp] 
    ffff    :: T.Direction -> IO[T.Direction]
    ffff d  =  matching =<< get_fieldStateArea (U.neighbor_area d p) state
      where
        matching (T.Puyo c' T.NotYet T.Normal)  | c == c'   = return [d]
        matching (T.Puyo c' T.EraseFlag _)      | c == c'   = return [d]
        matching _                                          = return []


--------------------------------------------------------------------------------
--  �G���A�`��    
--------------------------------------------------------------------------------
-- �t�B�[���h�̂Ղ�̕`��B�i�F�Ղ�E������܂Ղ�j
render_fieldPuyo    :: V.GameState -> T.Territory -> T.AnimationType 
                    -> T.AreaPosition -> GameObject -> IO()
render_fieldPuyo gs trt (T.Landing t pow) pos@(y, _) obj   =
    render_fieldObject' gs trt pos 0 moveY scaleX scaleY 0 obj
  where
    scaleX  = 1.2 * scaleX'
    moveY   = - unitAreaY gs / (scaleY * 4) * ( pow' )
    scaleY  = recip scaleX'
    scaleX' = (sqrt $ 1.01 * power - 1) * timeCoefficient + 1
      where
        power       = 1.1 + weaken 0.1 pow'
        timeCoefficient = ((halfTime - remTime) / halfTime) ^ 2 -- ���ԌW��
          where 
            halfTime    = fromIntegral W.amimeTime_land / 2 + 1
            remTime     = abs $ halfTime - fromIntegral t   -- �c�莞��
    weaken n t  = n * (t - 1)
    -- �ڒn�ʂ��t�B�[���h�����ɋ߂��ꍇ�̓p���[����߂�B
    pow'    | height == 0 && pow >= W.landingDefauletPower  = pow - 1
            | otherwise                                     = pow
      where
        height      = fieldSizeY - fromIntegral y - 1   -- ����
        fieldSizeY  = fromIntegral $ V.fieldSizeY' gs    
render_fieldPuyo gs trt (T.Erasing t)   p obj
    | t `rem` 8 >= 4    = return ()
    | otherwise         = render_fieldObject gs trt p obj
    
render_fieldPuyo gs trt (T.Dropping t) p obj =
    render_fieldObject' gs trt p 0 moveY 1 1 0 obj
  where
    moveY = fromIntegral W.amimeTime_drop * unitAreaY gs
                 / fromIntegral W.amimeTime_drop
render_fieldPuyo gs trt _               p obj = 
    render_fieldObject gs trt p obj

-- �t�B�[���h���W�ƃI�u�W�F�N�g���w�肵�āA���̈ʒu�ɕ`�悷��B
render_fieldObject  :: V.GameState -> T.Territory -> T.AreaPosition
                    -> GameObject -> IO()
render_fieldObject gs trt p obj = render_fieldObject' gs trt p 0 0 1 1 0 obj


render_fieldObject' :: V.GameState -> T.Territory -> T.AreaPosition ->
                        GLUT.GLdouble -> GLUT.GLdouble ->
                        GLUT.GLdouble -> GLUT.GLdouble ->
                        GLUT.GLdouble -> GameObject -> IO()
render_fieldObject' gs trt (y, x) mx my sx sy r obj = do
    let (y', x') = (fromIntegral y, fromIntegral x)
        posY = field_pointY     gs - (unitAreaY gs * (y' - 1) ) * 2
        posX = field_pointX trt gs + (unitAreaX gs * (x' - 1) ) * 2
        sx'  = sx * reviseViewSize gs
        sy'  = sy * reviseViewSize gs
    render_gameobject' (GLUT.Vector3 (posX + mx) (posY + my) 0) sx' sy' r obj