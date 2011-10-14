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
import Control.Monad.Instances

import qualified Common.Number          as Number
import Common.Color (Color)

--------------------------------------------------------------------------------
--  型
--------------------------------------------------------------------------------
data Score          = Score ActualScore Calculation

type ActualScore    = ( BaseType    -- おじゃまぷよに換算されたスコア
                      , BaseType )  -- おじゃまぷよに換算されていないスコア
                      
type BaseType       = Int

type Calculation    = ( Number.Chain        -- 連鎖数
                      , [Number.Union]      -- 連結数
                      , [Color]             -- 消したの色
                      )

--------------------------------------------------------------------------------
--  名前
--------------------------------------------------------------------------------
initial        = Score defaultActualScore defaultCalculation   :: Score

defaultActualScore  = (0, 0)        :: ActualScore
defaultCalculation  = (0, [], [])   :: Calculation

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
expandUniCol :: Number.Union -> Color -> Score -> Score
expandUniCol u c (Score sc (cha, uns, cls)) = (Score sc (cha, u:uns, c:cls))

-- 落下による得点
fallBounus                  :: Score -> Score
fallBounus  (Score act cal) =  Score (fmap (+1) act) cal

-- 消したぷよから得点を計算
calculate :: Score -> Score
calculate (Score score (cha, uns, cls)) = Score newScore (cha, [], [])
  where
    newScore    = fmap (+ 10 * basicBounus * sum uns) score
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
calculateBounusChain                :: Number.Chain -> BaseType
calculateBounusChain n  | n < 4     =  8 * truncate ((^^) 2 $ n - 2)
                        | otherwise =  (n - 3) * 32
                        
-- 連結ボーナスを算出する。
calculateBounusLink                 :: Number.Union -> BaseType
calculateBounusLink n   | n <= 4    =  0
                        | n >= 11   =  10
                        | otherwise =  n - 3
-- 複色ボーナスを算出する。
calculateBounusColor                :: Number.Colors -> BaseType
calculateBounusColor                =  (*) 3 . truncate . (^^) 2 .(+) (-2)

-- 予告ぷよを算出する。
calculateYokoku :: Score -> Int -> (Number.Puyo, Score)
calculateYokoku (Score (ss, ds) cal) rate = (n, (Score ((ds - m + ss), m) cal))
  where(n, m)  = ds `quotRem` rate

-- 表示する得点
display                     :: Score -> BaseType
display (Score (ss, ds) _)  =  ss + ds