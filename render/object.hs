-- renderObject.hs
module Render.Object
    (
    GameObject,
    
    unitAreaY',
    unitAreaX',
    
    render_gameobject,
    render_gameobject',
    
    objControlPuyoBack,
    objOjamaPuyo,
    objPuyo,
    objPuyo',
    objBackfield,
    objFallPoint,
    objYokokuLv1,
    objYokokuLv2,
    objYokokuLv3,
    objYokokuLv4,
    objYokokuLv5,
    objYokokuLv6,
    )
    where

import qualified Graphics.UI.GLUT   as GLUT

import qualified Typedata   as T
import qualified World      as W (window_sizeY, window_sizeX)

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
type GameObject = IO()

--------------------------------------------------------------------------------
--  ��{�`��ݒ�
--------------------------------------------------------------------------------
-- �P�G���A������̑傫���̊�l�B unitAreaY' = �E�B���h�E�̏c��(1.0) ���@16  
unitAreaY'  =  0.0625                           :: Double
unitAreaX'  =  unitAreaY' * recip aspect_ratio  :: Double

--  �E�B���h�E�T�C�Y�̏c���䗦�B
aspect_ratio    :: Double
aspect_ratio    =  (fromIntegral W.window_sizeX) / (fromIntegral W.window_sizeY)

--------------------------------------------------------------------------------
--  �I�u�W�F�N�g�`��    
--------------------------------------------------------------------------------
-- �Q�[���I�u�W�F�N�g��`�悷��B
render_gameobject :: GLUT.Vector3 Double -> GameObject -> IO()
render_gameobject vec3 obj  = render_gameobject' vec3 1 1 0 obj

-- �Q�[���I�u�W�F�N�g��`��i�g��k���E��]�Ή��B��]�͓x��P�ʂƂ������l������B�j
render_gameobject' :: GLUT.Vector3 Double 
                        -> GLUT.GLdouble -> GLUT.GLdouble -> GLUT.GLdouble 
                        -> GameObject -> IO()
render_gameobject' (GLUT.Vector3 x y z) sx sy r obj
    = GLUT.preservingMatrix $ do
        GLUT.translate (GLUT.Vector3 x y z :: GLUT.Vector3 Double)
        GLUT.rotate r (GLUT.Vector3 0.0 0.0 1.0 :: GLUT.Vector3 Double)
        GLUT.scale sx sy 1.0
        obj

--------------------------------------------------------------------------------
--  �Q�[���I�u�W�F�N�g    
--------------------------------------------------------------------------------
myVertex    = GLUT.vertex :: GLUT.Vertex3 GLUT.GLdouble -> IO()

-- ����Ղ�w�i
objControlPuyoBack  :: GameObject
objControlPuyoBack  =  do
    GLUT.color (GLUT.Color3 1.0 1.0 1.0 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Polygon
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 1.0) (unitAreaY' * 1.0) 0 0 0 12

-- ������܂Ղ�
objOjamaPuyo    :: GameObject
objOjamaPuyo    =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 0.5 0.5 0.5 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.LineLoop
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 0.9) (unitAreaY' * 0.9) 0 0 0 12

-- �Ղ�
objPuyo             :: T.Color -> GameObject
objPuyo color       =  do
    GLUT.color $ to_glutColor3 color        -- �F��ς���B
    GLUT.renderPrimitive GLUT.Polygon
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 0.93) (unitAreaY' * 0.93) 0 0 0 12

-- �Ղ�i�t�B�[���h��j
objPuyo'            :: T.Color -> [T.Direction] -> GameObject
objPuyo' color ds   =  do
    GLUT.color $ to_glutColor3 color        -- �F��ς���B
    GLUT.renderPrimitive GLUT.Polygon
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 0.93) (unitAreaY' * 0.93) 0 0 0 12
    mapM_ linkPuyo ds
  where
    -- �Ղ�̌q����
    linkPuyo    :: T.Direction -> GameObject
    linkPuyo T.DUp      =
        GLUT.renderPrimitive GLUT.Quads $ do 
            myVertex $ GLUT.Vertex3 (unitAreaX'  / sqrt 2 * 0.9) unitAreaY' 0
            myVertex $ GLUT.Vertex3 (-unitAreaX' / sqrt 2 * 0.9) unitAreaY' 0
            myVertex $ GLUT.Vertex3 (-unitAreaX' / sqrt 2) 0 0
            myVertex $ GLUT.Vertex3 (unitAreaX'  / sqrt 2) 0 0
    linkPuyo T.DDown    =
        GLUT.renderPrimitive GLUT.Quads $ do 
            myVertex $ GLUT.Vertex3 (unitAreaX'  / sqrt 2 * 0.9) (-unitAreaY') 0
            myVertex $ GLUT.Vertex3 (-unitAreaX' / sqrt 2 * 0.9) (-unitAreaY') 0
            myVertex $ GLUT.Vertex3 (-unitAreaX' / sqrt 2) 0 0
            myVertex $ GLUT.Vertex3 (unitAreaX'  / sqrt 2) 0 0
    linkPuyo T.DLeft    =
        GLUT.renderPrimitive GLUT.Quads $ do 
            myVertex $ GLUT.Vertex3 (-unitAreaX') (unitAreaY'  / sqrt 2 * 0.9) 0
            myVertex $ GLUT.Vertex3 (-unitAreaX') (-unitAreaY' / sqrt 2 * 0.9) 0
            myVertex $ GLUT.Vertex3 0 (-unitAreaY' / sqrt 2) 0
            myVertex $ GLUT.Vertex3 0 (unitAreaY'  / sqrt 2) 0
    linkPuyo T.DRight   =
        GLUT.renderPrimitive GLUT.Quads $ do 
            myVertex $ GLUT.Vertex3 unitAreaX' (unitAreaY'  / sqrt 2 * 0.9) 0
            myVertex $ GLUT.Vertex3 unitAreaX' (-unitAreaY' / sqrt 2 * 0.9) 0
            myVertex $ GLUT.Vertex3 0 (-unitAreaY' / sqrt 2) 0
            myVertex $ GLUT.Vertex3 0 (unitAreaY'  / sqrt 2) 0


-- �t�B�[���h�w�i
objBackfield    :: GameObject
objBackfield    =  do
    GLUT.color (GLUT.Color3 0.125 0.125 0.125 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Quads $ do
        myVertex $ GLUT.Vertex3 (unitAreaX'  * size) (unitAreaY'  * size) 0
        myVertex $ GLUT.Vertex3 (-unitAreaX' * size) (unitAreaY'  * size) 0
        myVertex $ GLUT.Vertex3 (-unitAreaX' * size) (-unitAreaY' * size) 0
        myVertex $ GLUT.Vertex3 (unitAreaX'  * size) (-unitAreaY' * size) 0 
  where
    size = 0.93

-- �����\���n�_
objFallPoint    :: T.Color -> GameObject
objFallPoint color   =  do
    GLUT.color $ to_glutColor3 color        -- �F��ς���B
    GLUT.renderPrimitive GLUT.Quads $ do
        myVertex $ GLUT.Vertex3 (unitAreaX'  * size) (unitAreaY'  * size) 0
        myVertex $ GLUT.Vertex3 (-unitAreaX' * size) (unitAreaY'  * size) 0
        myVertex $ GLUT.Vertex3 (-unitAreaX' * size) (-unitAreaY' * size) 0
        myVertex $ GLUT.Vertex3 (unitAreaX'  * size) (-unitAreaY' * size) 0
  where
    size = 0.2
    
-- �\���Ղ�
objYokokuLv1    :: GameObject
objYokokuLv1    =  do
    GLUT.lineWidth GLUT.$= 2.0
    GLUT.color (GLUT.Color3 0.5 0.5 0.5 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.LineLoop
        $ mapM_ GLUT.vertex 
            $ shape_circle sizeX sizeY 0 positionY 0 12
  where
    positionY   = negate unitAreaY' / 2
    sizeX       = unitAreaX' * 0.5
    sizeY       = unitAreaY' * 0.4
            
objYokokuLv2    :: GameObject     -- ������܂Ղ�Ɠ����B
objYokokuLv2    =  do
    GLUT.lineWidth GLUT.$= 3.0
    GLUT.color (GLUT.Color3 0.5 0.5 0.5 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.LineLoop
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 0.9) (unitAreaY' * 0.9) 0 0 0 12

objYokokuLv3    :: GameObject
objYokokuLv3    =  do
    GLUT.color (GLUT.Color3 0.5 0.5 0.5 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Polygon
        $ mapM_ GLUT.vertex 
            $ shape_circle (unitAreaX' * 0.9) (unitAreaY' * 0.9) 0 0 0 12

objYokokuLv4    :: GameObject
objYokokuLv4    =  do
    GLUT.color (GLUT.Color3 1.0 1.0 0.0 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Polygon   $ mapM_ GLUT.vertex star0
    GLUT.renderPrimitive GLUT.Triangles $ mapM_ GLUT.vertex star1
    GLUT.renderPrimitive GLUT.Triangles $ mapM_ GLUT.vertex star2
    GLUT.renderPrimitive GLUT.Triangles $ mapM_ GLUT.vertex star3
    GLUT.renderPrimitive GLUT.Triangles $ mapM_ GLUT.vertex star4
    GLUT.renderPrimitive GLUT.Triangles $ mapM_ GLUT.vertex star5
  where
    base   = shape_circle (unitAreaX' * 1.0) (unitAreaY' * 1.0) 0 0 0 10
    fff f (x:x':xs)   = x : f x' : fff f xs
    fff _ []          = []
    shorten n (GLUT.Vertex3 x y z)  = GLUT.Vertex3 (n * x) (n * y) (n * z)
    star    = fff (shorten 0.6) base
    star0   = (\[a,b,c,d,e,f,g,h,i,j] -> [b,d,f,h,j]) star
    star1   = (\[a,b,c,d,e,f,g,h,i,j] -> [b,c,d]) star
    star2   = (\[a,b,c,d,e,f,g,h,i,j] -> [d,e,f]) star
    star3   = (\[a,b,c,d,e,f,g,h,i,j] -> [f,g,h]) star
    star4   = (\[a,b,c,d,e,f,g,h,i,j] -> [h,i,j]) star
    star5   = (\[a,b,c,d,e,f,g,h,i,j] -> [j,a,b]) star
    
objYokokuLv5    :: GameObject
objYokokuLv5    =  do
    GLUT.color (GLUT.Color3 1.0 1.0 0.0 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Polygon $ mapM_ GLUT.vertex large
    GLUT.color (GLUT.Color3 0.0 0.0 0.0 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Polygon $ mapM_ GLUT.vertex small
  where
    large   = shape_circle (unitAreaX' * 0.9) (unitAreaY' * 0.9) 0  0  0 12
    small   = shape_circle (unitAreaX' * 0.6) (unitAreaY' * 0.6) pX pY 0 12
    pY   = unitAreaY' / 2
    pX   = unitAreaX' / 2
    
objYokokuLv6   :: GameObject
objYokokuLv6   =  do
    GLUT.color (GLUT.Color3 1.0 1.0 0.0 :: GLUT.Color3 Double)
    GLUT.renderPrimitive GLUT.Quads     base
    GLUT.renderPrimitive GLUT.Triangles tri1
    GLUT.renderPrimitive GLUT.Triangles tri2
    GLUT.renderPrimitive GLUT.Triangles tri3
 where
    base    = do
        myVertex $ GLUT.Vertex3 (es5 4) (-unitAreaY')   0
        myVertex $ GLUT.Vertex3 (es5 0) (-unitAreaY')   0
        myVertex $ GLUT.Vertex3 (es5 0) triPosYDown 0
        myVertex $ GLUT.Vertex3 (es5 4) triPosYDown 0
    es5 n   = (n - 2) * unitAreaX' * 0.9 / 2
    tri1    = do
        myVertex $ GLUT.Vertex3 (es5 0) triPosYUp   0
        myVertex $ GLUT.Vertex3 (es5 0) triPosYDown 0
        myVertex $ GLUT.Vertex3 (es5 1) triPosYDown 0
    tri2    = do
        myVertex $ GLUT.Vertex3 (es5 2) triPosYUp   0
        myVertex $ GLUT.Vertex3 (es5 1) triPosYDown 0
        myVertex $ GLUT.Vertex3 (es5 3) triPosYDown 0
    tri3    = do
        myVertex $ GLUT.Vertex3 (es5 4) triPosYUp   0
        myVertex $ GLUT.Vertex3 (es5 3) triPosYDown 0
        myVertex $ GLUT.Vertex3 (es5 4) triPosYDown 0
    triPosYUp   = unitAreaY' / 3
    triPosYDown = -unitAreaY' / 3
    
--------------------------------------------------------------------------------
-- �F
--------------------------------------------------------------------------------
to_glutColor3       :: T.Color -> GLUT.Color3 Double
to_glutColor3 T.Red     = GLUT.Color3 1.0 0.0 0.0
to_glutColor3 T.Green   = GLUT.Color3 0.0 1.0 0.2
to_glutColor3 T.Blue    = GLUT.Color3 0.0 0.3 1.0
to_glutColor3 T.Yellow  = GLUT.Color3 1.0 1.0 0.0
to_glutColor3 T.Purple  = GLUT.Color3 0.7 0.0 1.0
--to_glutColor3 T.SkyBlue = GLUT.Color3 0.0 0.7 1.0

--------------------------------------------------------------------------------
--  ��{�`
--------------------------------------------------------------------------------
-- X���̒���-> y���̒���-> ���S�_x���W-> ���S�_y���W-> ���_�̊p�x-> ���_�̐�-> 
shape_circle    :: Double -> Double -> Double -> Double -> Double -> Double
                    -> [GLUT.Vertex3 GLUT.GLdouble]
shape_circle rx ry mx my an n
 = move_shape mx my [GLUT.Vertex3(ff cos rx v)(ff sin ry v)0 | v <- [0..(n-1)]]
 where
    ff f' r' v' = r' * (f' $ (an/180 + v') / (n/2)*pi)

-- ��}�����}�`�𕽖ʈړ�����B
move_shape  :: Double -> Double -> [GLUT.Vertex3 GLUT.GLdouble]
                -> [GLUT.Vertex3 GLUT.GLdouble]
move_shape x y vs
 =  map (\(GLUT.Vertex3 vx vy vz) -> GLUT.Vertex3 (x + vx) (y + vy) (vz)) vs