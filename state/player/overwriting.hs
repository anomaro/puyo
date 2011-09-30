module State.Player.Overwriting
    (   
    -- �Q�[����ԑJ�ڍX�V
    shift_gamePhase,
    
    -- �l�N�X�g�Ղ��ԍX�V
    eat_nextPuyo,
    
    -- �t�B�[���h��ԍX�V
    renew_fieldArea,
    renew_animationType,
    
    -- �z�Ղ��ԍX�V
    renew_playerPuyo,
    renew_playerPuyo',
    remove_playerPuyo,
    
    -- ���_�X�V
    renewScoreCalculation,
    renewScore,
    calculateScoreD,
    
    -- �\���Ղ�X�V
    renewYokoku,
    toSupplyYokoku,
    toAdvanceYokoku,
    
    -- �s�k�t���O�X�V
    renewLoseFlag,
    )
    where

import State.Player.Substance
import State.Player.Query

import Data.List    (nub)
import Data.Maybe   (fromMaybe)
import Control.Monad
import Control.Applicative
import qualified Data.IORef         as IORF
import qualified Data.Array.IO      as AIO

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--  ��ԏ�������
--------------------------------------------------------------------------------
-- �Q�[����Ԃ�ς���B
shift_gamePhase                 :: PlayerState -> T.GamePhase -> IO()
shift_gamePhase state gamephase =
    flip IORF.writeIORef gamephase =<< takeout_gamePhaseState state

-- �l�N�X�g�Ղ�������B
eat_nextPuyo    :: PlayerState -> T.NumOfPuyo -> IO()
eat_nextPuyo state n =
    flip IORF.modifyIORef (drop n) =<< takeout_nextPuyoState state
    
--------------------------------------------------------------------------------
--  �s�k�t���O�X�V
--------------------------------------------------------------------------------
-- �s�k�t���O���X�V����B
renewLoseFlag               :: Bool -> T.Territory -> PlayerState -> IO()
renewLoseFlag b trt state   =  do
    loseFlag    <- IORF.readIORef refLoseFlag
    IORF.writeIORef refLoseFlag $ applyTRT trt (const b) loseFlag
  where
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    refLoseFlag = takeout_loseFlagState state

--------------------------------------------------------------------------------
--  �\���Ղ�X�V
--------------------------------------------------------------------------------
-- �\���Ղ���X�V����B
renewYokoku :: (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> (T.NumOfPuyo -> T.NumOfPuyo)
            -> T.Territory -> PlayerState -> IO()
renewYokoku f1 f2 f3 trt state   = do
    yokokuField <- IORF.readIORef $ takeout_yokokuState state
    IORF.writeIORef (takeout_yokokuState state) $ applyTRT trt yokokuField
  where
    applyTRT T.TerritoryLeft  (a, b) = (apply a, b      )
    applyTRT T.TerritoryRight (a, b) = (a      , apply b)
    apply (n1, n2, n3) = (fmap f1 n1, fmap f2 n2, fmap f3 n3)

-- �\���Ղ��Reserve����Supply�ֈڂ��B
toSupplyYokoku :: T.Territory -> PlayerState -> IO()
toSupplyYokoku trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ applyTRT trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    shift (n, Supply n2, Reserve n3)    = (n, Supply $ n2 + n3, Reserve 0)

-- �\���Ղ��Supply����Advance�ֈڂ��B
toAdvanceYokoku :: T.NumOfPuyo -> T.Territory -> PlayerState -> IO()
toAdvanceYokoku m trt state =  do
    yokokuField <- IORF.readIORef stateY
    IORF.writeIORef stateY $ applyTRT trt shift yokokuField
  where
    stateY  = takeout_yokokuState state
    applyTRT T.TerritoryLeft  f (l, r)  = (f l, r  )
    applyTRT T.TerritoryRight f (l, r)  = (l  , f r)
    shift (Advance n1, Supply n2, n)
        | n2 > m    = (Advance $ n1 + m , Supply $ n2 - m, n)
        | otherwise = (Advance $ n1 + n2, Supply 0       , n)

--------------------------------------------------------------------------------
--  ���_�X�V
--------------------------------------------------------------------------------
-- ���_��ScoreCalculation������������B
renewScoreCalculation :: (T.NumOfChain -> T.NumOfChain) 
                      -> ([T.NumOfUnion] -> [T.NumOfUnion]) 
                      -> ([T.Color] -> [T.Color]) 
                      -> PlayerState -> IO()
renewScoreCalculation f f' f'' state =  do
    (score, (chain, ns, cs) ) <- IORF.readIORef $ takeout_scoreState state
    IORF.writeIORef (takeout_scoreState state)
                    ( score, (f chain, f' ns, f'' cs) )

-- ���_������������B
renewScore  :: (T.StaticScore -> T.StaticScore)
            -> (T.DynamicScore -> T.DynamicScore)
            -> PlayerState -> IO()
renewScore f f' state   =  do
    (T.Score ss sd, sc )    <- IORF.readIORef $ takeout_scoreState state
    IORF.writeIORef (takeout_scoreState state) ( T.Score (f ss) (f' sd), sc )

-- ���_��ScoreCalculation���v�Z���āADynamicScore������������B
calculateScoreD :: PlayerState -> IO()
calculateScoreD state   =  do
    (score, (chain, ns, cs) ) <- IORF.readIORef $ takeout_scoreState state
    let basicbounus = calculateBasicBounus  
                        $ calculateBounusChain chain
                        + (calculateBounusColor $ length $ nub $ cs)
                        + (sum $ map calculateBounusLink ns)
        newScore    = score .+ 10 * basicbounus * sum ns
    IORF.writeIORef (takeout_scoreState state)
                    (newScore, (chain, [], []))

-- ��{�{�[�i�X�l���Z�o����B
calculateBasicBounus                :: Int -> Int
calculateBasicBounus n  | n <= 0    =  1
                        | n >  1000 =  999
                        | otherwise =  n
-- �A���{�[�i�X���Z�o����B
calculateBounusChain                :: Int -> Int
calculateBounusChain n  | n < 4     =  8 * truncate ((^^) 2 $ n - 2)
                        | otherwise =  (n - 3) * 32
                        
-- �A���{�[�i�X���Z�o����B
calculateBounusLink                 :: Int -> Int
calculateBounusLink n   | n <= 4    =  0
                        | n >= 11   =  10
                        | otherwise =  n - 3
-- ���F�{�[�i�X���Z�o����B
calculateBounusColor                :: Int -> Int
calculateBounusColor                =  (*) 3 . truncate . (^^) 2 .(+) (-2)

-- ������܂Ղ�Ɋ��Z����Ă��Ȃ��X�R�A�ɑ����B
(.+) :: T.Score -> T.ScoreBaseType -> T.Score
(T.Score ss sd) .+ n    = T.Score ss $ sd + n
infixl 5 .+

--  1 	2 	3 	4 	5 	6 	7 	8 	9 	10 	11 	12 	13 	14 	15 	16 	17 	18 	19
--  0 	8 	16 	32 	64 	128 256 512 999 999 999 999 999 999 999 999 999 999 999
--  0 	8 	16 	32 	64 	96 	128 160 192 224 256 288 320 352 384 416 448 480 512

--------------------------------------------------------------------------------
--  �t�B�[���h��ԍX�V
--------------------------------------------------------------------------------
-- �G���A���X�V����B
renew_fieldArea :: PlayerState -> T.AreaPosition -> T.Area -> IO()
renew_fieldArea state p area    =
    takeout_fieldstate state >>= \stateF -> AIO.writeArray stateF p area

-- �A�j���[�V������Ԃ��X�V����B
renew_animationType :: PlayerState -> T.AreaPosition -> IO()
renew_animationType state p =  do
    stateF  <- takeout_fieldstate state
    area    <- get_fieldStateArea p state
    AIO.writeArray stateF p $ countArea area

countArea :: T.Area -> T.Area
countArea (T.Puyo c uc at)  = T.Puyo c uc $ countAT at
countArea (T.Ojama  uc at)  = T.Ojama  uc $ countAT at
countArea a                 = a

countAT :: T.AnimationType -> T.AnimationType
countAT ( T.Dropping n   )  | n > 0 = T.Dropping $ n - 1
countAT ( T.Landing  n p )  | n > 0 = T.Landing  ( n - 1 ) p
countAT ( T.Erasing  n   )  | n > 0 = T.Erasing  $ n - 1
countAT _                           = T.Normal

--------------------------------------------------------------------------------
--  �z�Ղ��ԍX�V
--------------------------------------------------------------------------------
-- ������������
renew_playerPuyo :: PlayerState 
                    -> Maybe (T.Color, T.Color) -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> Maybe T.AreaPosition     -- ��_�Ղ�̃t�B�[���h���W
                    -> Maybe T.Direction        -- ���_�Ղ�̕���
                    -> Maybe T.Time             -- ���R�����p�̃J�E���^
                    -> Maybe T.Time             -- ��]�p�̃J�E���^
                    -> Maybe Bool               -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo state c p d tf tt qf   = do
    stateP                              <- takeout_ppuyostate state
    PlayerPuyoInfo c' p' d' tf' tt' qf' <- IORF.readIORef stateP
    IORF.writeIORef stateP $ PlayerPuyoInfo (fromMaybe c'  c )
                                            (fromMaybe p'  p )
                                            (fromMaybe d'  d )
                                            (fromMaybe tf' tf)
                                            (fromMaybe tt' tt)
                                            (fromMaybe qf' qf)

-- ���S��������
renew_playerPuyo'   :: PlayerState 
                    -> (T.Color, T.Color) -- �i��_�Ղ�̐F�A���_�Ղ�̐F�j
                    -> T.AreaPosition     -- ��_�Ղ�̃t�B�[���h���W
                    -> T.Direction        -- ���_�Ղ�̕���
                    -> T.Time             -- ���R�����p�̃J�E���^
                    -> T.Time             -- ��]�p�̃J�E���^
                    -> Bool               -- �N�C�b�N�^�[���t���O
                    -> IO ()
renew_playerPuyo' state c p d tf tt qf  =
    flip IORF.writeIORef (PlayerPuyoInfo c p d tf tt qf) 
                                        =<< takeout_ppuyostate state

-- ���S��������
remove_playerPuyo :: PlayerState -> IO()
remove_playerPuyo =  flip IORF.writeIORef NonExistent <=< takeout_ppuyostate