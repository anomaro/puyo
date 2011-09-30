module State.Player.Query
    (
    -- �v���[���[���ʎq�擾�B
    get_playerIdentity,
    
    -- �Q�[����Ԏ擾
    get_gamePhase,
    
    -- �t�B�[���h�̏�Ԏ擾
    get_fieldStateArea,
    is_neighborSpace,
    
    -- ����Ղ�̏�Ԏ擾
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    get_PlayerPuyoFallTime,
    get_PlayerPuyoRotateTime,
    get_PlayerPuyoQuickTurnFlag,
    
    -- �l�N�X�g�Ղ�̐F�擾
    get_nextPuyoColors,
    
    -- ���_�擾
    get_score,
    
    -- �\���Ղ�擾
    get_fallOjamaPuyo,
    get_yokoku,
    
    -- �s�k�t���O
    get_whoWins,
    
    -- ��Ԑ���
    create_playerstate,
    create_nextPuyoState,
    copy_nextPuyoState,
    create_yokokuState,
    create_loseFlagState,
    )
    where


import qualified State.Player.Substance as P'

import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V

import qualified Common.PlayerIdentity  as Identity

(<$<) :: Functor f => (a -> b) -> (c -> f a) -> (c -> f b)
f <$< g =  fmap f . g
infixr 1 <$<

readField   :: (AIO.MArray a e m, AIO.Ix i) => i -> a i e -> m e
readField   =  flip AIO.readArray 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  �v���C���[��Ԃ̓��e���O�����W���[���ɓ`����B
--------------------------------------------------------------------------------
-- �v���C���[���ʂ�`����B
get_playerIdentity  :: P'.PlayerState -> Identity.PlayerIdentity
get_playerIdentity  =  P'.takeout_playerIdentity

-- �Q�[����Ԃ�`����B
get_gamePhase   :: P'.PlayerState -> IO T.GamePhase
get_gamePhase   =  IORF.readIORef <=< P'.takeout_gamePhaseState

-- �l�N�X�g�Ղ�̐F��`����B
get_nextPuyoColors  :: P'.PlayerState -> IO [T.Color]
get_nextPuyoColors  =  IORF.readIORef <=< P'.takeout_nextPuyoState

--------------------------------------------------------------------------------
--  �s�k�t���O
--------------------------------------------------------------------------------
-- ���������v���C���[�𒲂ׂ�B
get_whoWins         :: P'.PlayerState -> IO (Maybe Identity.Territory)
get_whoWins state   =  do
    loseFlag    <- IORF.readIORef $ P'.takeout_loseFlagState state
    return $ check loseFlag
  where
    check (False, True)     = Just Identity.Left
    check (True,  False)    = Just Identity.Right
    check _                 = Nothing

--------------------------------------------------------------------------------
--  �\���Ղ�
--------------------------------------------------------------------------------
-- ���ۂɍ~�邨����܂Ղ�̐���`����B
get_fallOjamaPuyo   :: Identity.Territory -> P'.PlayerState -> IO T.NumOfPuyo
get_fallOjamaPuyo trt state = do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ get $ (Identity.pick trt) yokokuField
  where
    get (P'.Advance n, _, _)    = n

-- �\���Ղ�̐���`����B
get_yokoku  :: Identity.Territory -> P'.PlayerState -> IO T.NumOfPuyo
get_yokoku trt state =  do
    yokokuField <- IORF.readIORef $ P'.takeout_yokokuState state
    return $ totalYokoku $ (Identity.pick trt) yokokuField
  where
    totalYokoku (P'.Advance n1, P'.Supply n2, P'.Reserve n3) = n1 + n2 + n3
    
--------------------------------------------------------------------------------
--  ���_
--------------------------------------------------------------------------------
-- ���_��`����B
get_score       :: P'.PlayerState -> IO T.Score
get_score state =  do
    scores   <- IORF.readIORef $ P'.takeout_scoreState state
    return $ fst scores

--------------------------------------------------------------------------------
--  �t�B�[���h�̏�Ԏ擾
--------------------------------------------------------------------------------
-- �w�肵���G���A�̃t�B�[���h�̃I�u�W�F�N�g�̎�ނ�`����B
get_fieldStateArea      :: T.AreaPosition -> P'.PlayerState -> IO T.Area
get_fieldStateArea p    =  readField p <=< P'.takeout_fieldstate
                             
-- �w�肵�������ɗאڂ���G���A�̃I�u�W�F�N�g���A�󔒂��ǂ�������B
is_neighborSpace :: T.AreaPosition -> T.Direction -> P'.PlayerState -> IO Bool
is_neighborSpace p d =
    (T.Space ==) <$< readField (U.neighbor_area d p) <=< P'.takeout_fieldstate

--------------------------------------------------------------------------------
--  ����Ղ�̏�Ԏ擾
--------------------------------------------------------------------------------
-- ����Ղ悪���݂��邩�ǂ����`����B
get_PlayerPuyoExistent  :: P'.PlayerState -> IO Bool
get_PlayerPuyoExistent  =  (P'.NonExistent /=) <$< readTakePP

-- ����Ղ�̐F�̑g�݂�`����B
get_PlayerPuyoColors        :: P'.PlayerState -> IO (T.Color,T.Color)
get_PlayerPuyoColors        =  getPPColor           <$< readTakePP

-- ����Ղ�̊�_�Ղ�̃t�B�[���h���W��`����B
get_PlayerPuyoPosition      :: P'.PlayerState -> IO T.AreaPosition
get_PlayerPuyoPosition      =  getPPPosition        <$< readTakePP

-- ����Ղ�̓��_�Ղ�̕�����`����B
get_PlayerPuyoDirection     :: P'.PlayerState -> IO T.Direction
get_PlayerPuyoDirection     =  getPPDirection       <$< readTakePP

-- ����Ղ�̎��R�����p�̃J�E���^��`����B
get_PlayerPuyoFallTime      :: P'.PlayerState -> IO T.Time
get_PlayerPuyoFallTime      =  getPPFallTime        <$< readTakePP

-- ����Ղ�̉�]�p�̃J�E���^��`����B
get_PlayerPuyoRotateTime    :: P'.PlayerState -> IO T.Time
get_PlayerPuyoRotateTime    =  getPPRotateTime      <$< readTakePP

-- ����Ղ�̃N�C�b�N�^�[���̉ۂ�`����B
get_PlayerPuyoQuickTurnFlag :: P'.PlayerState -> IO Bool
get_PlayerPuyoQuickTurnFlag =  getPPFlagQuickTurn   <$< readTakePP


readTakePP :: P'.PlayerState -> IO P'.PlayerPuyo
readTakePP =  IORF.readIORef <=< P'.takeout_ppuyostate

getPPColor          (P'.PlayerPuyoInfo c _ _ _ _ _)    = c :: (T.Color,T.Color)
getPPPosition       (P'.PlayerPuyoInfo _ p _ _ _ _)    = p :: T.AreaPosition
getPPDirection      (P'.PlayerPuyoInfo _ _ d _ _ _)    = d :: T.Direction
getPPFallTime       (P'.PlayerPuyoInfo _ _ _ f _ _)    = f :: T.Time
getPPRotateTime     (P'.PlayerPuyoInfo _ _ _ _ r _)    = r :: T.Time
getPPFlagQuickTurn  (P'.PlayerPuyoInfo _ _ _ _ _ q)    = q :: Bool


--------------------------------------------------------------------------------
--  �v���C���[��ԏ�����
--------------------------------------------------------------------------------
create_playerstate      :: Identity.PlayerIdentity
                        -> V.GameState -> P'.NextPuyoState
                        -> P'.YokokuState -> P'.LoseFlagState
                        -> IO P'.PlayerState
create_playerstate pI gs stateN stateY stateL   =  do
    stateG  <- create_gamePhaseState
    stateF  <- create_fieldstate
    stateP  <- create_ppuyostate
    stateS  <- create_scorestate
    return $ P'.PlayerState pI stateG stateF stateP stateN stateS stateY stateL
  where
    -- ���_�̏������
    create_scorestate   =  IORF.newIORef initalScore
    initalScore = ( (T.Score 0 0), P'.defaultScoreCalculation )
    -- �Q�[����Ԃ��������B
    create_gamePhaseState   :: IO P'.GamePhaseState
    create_gamePhaseState   =  IORF.newIORef T.BuildPhase 
    -- ����Ղ�̏�ԕϐ����������B
    create_ppuyostate   :: IO P'.PlayerPuyoState
    create_ppuyostate   =  IORF.newIORef P'.NonExistent
    -- �t�B�[���h��Ԃ̉ϔz����쐬����B
    create_fieldstate :: IO P'.FieldState
    create_fieldstate = 
        AIO.newArray ((1, 1), (V.fieldSizeY' gs, V.fieldSizeX' gs)) T.Space
        >>= \stateF -> build_wall stateF    -- �ǃI�u�W�F�N�g�����B
        >> return stateF
      where
        build_wall          :: P'.FieldState -> IO()
        build_wall stateF   =  do  
            writeStroke_field stateF walls (fieldArray_indicesY $ V.fieldSizeY' gs)
            writeStroke_field stateF walls (fieldArray_indicesX $ V.fieldSizeX' gs)
            writeStroke_field stateF walls (fieldArray_indicesX 1)
          where
            walls = repeat T.Wall
            -- �t�B�[���h�́A���X�g���瓾�����W�����̃I�u�W�F�N�g�ɏ���������B
            writeStroke_field :: P'.FieldState -> [T.Area] -> [T.AreaPosition] -> IO()
            writeStroke_field _      _      []      = return ()
            writeStroke_field _      []     _       = return ()
            writeStroke_field stateF (a:as) (p:ps)  =
                AIO.writeArray stateF p a
                >> writeStroke_field stateF as ps
            -- �t�B�[���h��Ԕz��́AY���W���w�肵���v�f�̃��X�g�B�iX���W�͑���Ղ�̉��͈́j
            fieldArray_indicesY :: T.PositionY -> [T.AreaPosition]
            fieldArray_indicesY y   
             | 1 <= y && y <= V.fieldSizeY' gs  = [(y, x) | x <- [2..V.fieldSizeX' gs - 1] ]
             | otherwise                        = []
            -- �t�B�[���h��Ԕz��́AX���W���w�肵���v�f�̃��X�g�B
            fieldArray_indicesX :: T.PositionX -> [T.AreaPosition]
            fieldArray_indicesX x   
             | 1 <= x && x <= V.fieldSizeX' gs  = [(y, x) | y <- [1..V.fieldSizeY' gs] ]
             | otherwise                        = []


-- �l�N�X�g�Ղ���������B
create_nextPuyoState        :: V.GameState -> IO P'.NextPuyoState
create_nextPuyoState  gs    =  do
    seed    <- U.runRandom maxBound
    colors  <- V.get_ColorPattern gs
    IORF.newIORef 
        $ map (V.makeColor colors) $ U.makeRandoms (V.get V.Color gs - 1) seed
        
-- �l�N�X�g�Ղ���R�s�[����B�i�Q�o���̃l�N�X�g�Ղ��Ԃ����Ƃ��Ɏg���B�j
copy_nextPuyoState :: P'.NextPuyoState -> IO P'.NextPuyoState
copy_nextPuyoState =  IORF.newIORef <=< IORF.readIORef  


-- �\���Ղ���������B
create_yokokuState :: IO P'.YokokuState
create_yokokuState =  IORF.newIORef P'.defaultYokokuField

-- �s�k�t���O���������B
create_loseFlagState    :: IO P'.LoseFlagState
create_loseFlagState    =  IORF.newIORef (False, False)
