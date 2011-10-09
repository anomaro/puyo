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
--  型
--------------------------------------------------------------------------------
data Score          = Score ActualScore Calculation

data ActualScore    = ActualScore
                        { static   :: BaseType  -- おじゃまぷよに換算されたスコア
                        , dynamic  :: BaseType  -- おじゃまぷよに換算されていないスコア
                        }
type BaseType  = Int           -- スコアの数値の型。 ※2147483647

type Calculation   = ( T.NumOfChain     -- 連鎖数
                     , [T.NumOfUnion]   -- 連結数
                     , [T.Color]        -- 消したの色
                     )

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initial        = Score defaultActualScore defaultCalculation   :: Score

defaultActualScore  = ActualScore 0 0   :: ActualScore
defaultCalculation  = (0, [], [])       :: Calculation

--------------------------------------------------------------------------------
--  関数
--------------------------------------------------------------------------------
-- 計算用メモを初期化
refresh                 :: Score -> Score
refresh (Score act _)   =  Score act defaultCalculation

-- 連鎖数を集計
expandChain                                 :: Score -> Score
expandChain (Score sc (cha, _, _))          =  Score sc (cha + 1, [], [])

-- 連結数・色の数を集計
expandUniCol :: T.NumOfUnion -> T.Color -> Score -> Score
expandUniCol u c (Score sc (cha, uns, cls)) = (Score sc (cha, u:uns, c:cls))

-- 落下による得点
fallBounus                  :: Score -> Score
fallBounus  (Score act cal) =  Score (act .+ 1) cal

-- 消したぷよから得点を計算
calculate :: Score -> Score
calculate (Score score (cha, uns, cls)) = Score newScore (cha, [], [])
  where
    newScore    = score .+ 10 * basicBounus * sum uns
    basicBounus = calculateBasicBounus $ chab + clsb + lnkb
    chab        = calculateBounusChain cha
    clsb        = calculateBounusColor . length . nub $ cls
    lnkb        = sum . map calculateBounusLink $ uns

-- 基本ボーナス値を算出する。
calculateBasicBounus                :: BaseType  -> BaseType
calculateBasicBounus n  | n <= 0    =  1
                        | n >  1000 =  999
                        | otherwise =  n
-- 連鎖ボーナスを算出する。
calculateBounusChain                :: T.NumOfChain -> BaseType
calculateBounusChain n  | n < 4     =  8 * truncate ((^^) 2 $ n - 2)
                        | otherwise =  (n - 3) * 32
                        
-- 連結ボーナスを算出する。
calculateBounusLink                 :: T.NumOfUnion -> BaseType
calculateBounusLink n   | n <= 4    =  0
                        | n >= 11   =  10
                        | otherwise =  n - 3
-- 複色ボーナスを算出する。
calculateBounusColor                :: T.NumOfColors -> BaseType
calculateBounusColor                =  (*) 3 . truncate . (^^) 2 .(+) (-2)

-- 予告ぷよを算出する。
calculateYokoku :: Score -> Int -> (Int, Score)
calculateYokoku (Score (ActualScore ss ds) cal) rate =
    (n, (Score (ActualScore (ds - m + ss) m) cal))
  where(n, m)  = ds `quotRem` rate

-- 表示する得点
display                                 :: Score -> BaseType
display (Score (ActualScore ss ds) _)   =  ss + ds

-- おじゃまぷよに換算されていないスコアに足す。
(.+) :: ActualScore -> BaseType -> ActualScore
(ActualScore ss sd) .+ n    = ActualScore ss $ sd + n
infixl 5 .+