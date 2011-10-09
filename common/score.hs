module Common.Score
( Score
, initial
, refresh
, expandChain
, expandUniCol
, display
, fallBounus
, calculate
, calculateYokoku
) where

import Data.List (nub)
import qualified Common.DataType   as T

--------------------------------------------------------------------------------
--  �^
--------------------------------------------------------------------------------
data Score          = Score ActualScore Calculation

data ActualScore    = ActualScore
                        { static   :: BaseType  -- ������܂Ղ�Ɋ��Z���ꂽ�X�R�A
                        , dynamic  :: BaseType  -- ������܂Ղ�Ɋ��Z����Ă��Ȃ��X�R�A
                        }
type BaseType  = Int           -- �X�R�A�̐��l�̌^�B ��2147483647

type Calculation   = ( T.NumOfChain     -- �A����
                     , [T.NumOfUnion]   -- �A����
                     , [T.Color]        -- �������̐F
                     )

--------------------------------------------------------------------------------
--  ���O
--------------------------------------------------------------------------------
initial        = Score defaultActualScore defaultCalculation   :: Score

defaultActualScore  = ActualScore 0 0   :: ActualScore
defaultCalculation  = (0, [], [])       :: Calculation

--------------------------------------------------------------------------------
--  �֐�
--------------------------------------------------------------------------------
-- �v�Z�p������������
refresh                 :: Score -> Score
refresh (Score act _)   =  Score act defaultCalculation

-- �A�������W�v
expandChain                                 :: Score -> Score
expandChain (Score sc (cha, _, _))          =  Score sc (cha + 1, [], [])

-- �A�����E�F�̐����W�v
expandUniCol :: T.NumOfUnion -> T.Color -> Score -> Score
expandUniCol u c (Score sc (cha, uns, cls)) = (Score sc (cha, u:uns, c:cls))

-- �����ɂ�链�_
fallBounus                  :: Score -> Score
fallBounus  (Score act cal) =  Score (act .+ 1) cal

-- �������Ղ悩�瓾�_���v�Z
calculate :: Score -> Score
calculate (Score score (cha, uns, cls)) = Score newScore (cha, [], [])
  where
    newScore    = score .+ 10 * basicBounus * sum uns
    basicBounus = calculateBasicBounus $ chab + clsb + lnkb
    chab        = calculateBounusChain cha
    clsb        = calculateBounusColor . length . nub $ cls
    lnkb        = sum . map calculateBounusLink $ uns

-- ��{�{�[�i�X�l���Z�o����B
calculateBasicBounus                :: BaseType  -> BaseType
calculateBasicBounus n  | n <= 0    =  1
                        | n >  1000 =  999
                        | otherwise =  n
-- �A���{�[�i�X���Z�o����B
calculateBounusChain                :: T.NumOfChain -> BaseType
calculateBounusChain n  | n < 4     =  8 * truncate ((^^) 2 $ n - 2)
                        | otherwise =  (n - 3) * 32
                        
-- �A���{�[�i�X���Z�o����B
calculateBounusLink                 :: T.NumOfUnion -> BaseType
calculateBounusLink n   | n <= 4    =  0
                        | n >= 11   =  10
                        | otherwise =  n - 3
-- ���F�{�[�i�X���Z�o����B
calculateBounusColor                :: T.NumOfColors -> BaseType
calculateBounusColor                =  (*) 3 . truncate . (^^) 2 .(+) (-2)

-- �\���Ղ���Z�o����B
calculateYokoku :: Score -> Int -> (Int, Score)
calculateYokoku (Score (ActualScore ss ds) cal) rate =
    (n, (Score (ActualScore (ds - m + ss) m) cal))
  where(n, m)  = ds `quotRem` rate

-- �\�����链�_
display                                 :: Score -> BaseType
display (Score (ActualScore ss ds) _)   =  ss + ds

-- ������܂Ղ�Ɋ��Z����Ă��Ȃ��X�R�A�ɑ����B
(.+) :: ActualScore -> BaseType -> ActualScore
(ActualScore ss sd) .+ n    = ActualScore ss $ sd + n
infixl 5 .+