module Process.Phase.Erase
( erase_puyo
, rewriteSpase_puyo
) where

import Control.Monad (foldM, when, mapM_)

import State.Player.DataType
import State.Player.Query
import State.Player.Overwriting
import Standardizable
import qualified Data.PlayerIdentity  as Identity (against, territory)
import qualified Data.Area            as Area
import qualified Data.Direction       as Direction
import qualified Data.Score           as Score
import Data.Color (Color)
import qualified Data.Field           as Field
import qualified Data.Yokoku          as Yokoku (add)
import qualified Data.Number          as Number
import qualified Data.Setting          as Setting

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 結合状態を調べてぷよを消滅させる。ぷよが消えた場合True
erase_puyo          :: Setting.Setting -> PlayerState -> IO Bool
erase_puyo gs state =  do
    renewScore Score.expandChain state
    foldM f False $ Field.arrayIndices gs
  where
    f :: Bool -> Field.Position -> IO Bool
    f b p = do
        area        <- get_fieldStateArea p state 
        numUnion    <- count $ Area.isUnionCheck Nothing area p
        when (newBool numUnion) $ erace numUnion
        off_unionCheck state p
        return $ b || (newBool numUnion)
      where
        count False = return 0
        count True  = check_union state p
        newBool = (>= Setting.get Setting.ErasePuyo gs)
        erace n = do
            area <- get_fieldStateArea p state
            erase_unionPuyo state p
            renewScore (Score.expandUniCol n $ Area.color area) state

-- 結合状態を調べてぷよを消滅させる。（消滅フラグが立っているエリアを空白にする）
rewriteSpase_puyo           :: Setting.Setting -> PlayerState -> IO ()
rewriteSpase_puyo gs state  =  do
    mapM_ f $ Field.arrayIndices gs
    renewScore Score.calculate state
    calculateYokoku
  where
    f :: Field.Position -> IO()
    f p = do
        area    <- get_fieldStateArea p state
        when (Area.isReplacedSpace area p) $ renew_fieldArea state p standard
    -- 予告ぷよを算出する。
    calculateYokoku =  do
        score <- get_score state
        let (n, newScore)  = Score.calculateYokoku score
                                (Setting.get Setting.OjamaRate gs)
        renewYokoku (Yokoku.add n) trt state
        renewScore (const newScore) state
      where
        trt = Identity.against . Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
-- 結合チェック状態を未調査状態にする。
off_unionCheck :: PlayerState -> Field.Position -> IO()
off_unionCheck state p = do
    area <- get_fieldStateArea p state
    when (Area.isUnionCheckFinished Nothing area p)
        $ renew_fieldArea state p $ Area.modifyUnion (const standard) area

-- 結合しているぷよを消滅させる。（消滅フラグを立てる。）
erase_unionPuyo :: PlayerState -> Field.Position -> IO()
erase_unionPuyo state p = do
    area <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.eracingPuyo (Just $ Area.color area)
    mapM_ (fff $ Area.color area) Direction.areas
  where
    fff :: Color -> Direction.Area -> IO()
    fff color d = do
        area    <- get_fieldStateArea p' state
        when (Area.isEraseOjamaPuyo area) $ eraseOjamaPuyo  state p'
        when (Area.isUnionCheckFinished (Just color) area p') 
                                          $ erase_unionPuyo state p'
      where
        p'  = Field.neighbor d p
        -- おじゃまぷよを消滅させる。（消滅フラグを立てる。）
        eraseOjamaPuyo      :: PlayerState -> Field.Position -> IO()
        eraseOjamaPuyo s p  =  renew_fieldArea s p $ Area.eracingPuyo Nothing

-- エリア対象の結合チェック。このエリアがチェック対象かどうかは事前に調べてある。
check_union :: PlayerState -> Field.Position -> IO Number.Union
check_union state p    = do
    area <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.unionCheckCompletion area
    foldM (fff $ Area.color area) 1 Direction.areas
  where
    fff :: Color -> Number.Union -> Direction.Area -> IO Number.Union
    fff color n d = do
        area    <- get_fieldStateArea p' state
        if Area.isUnionCheck (Just color) area p'
          then check_union state p' >>= return . (+ n)
          else return n
      where
        p'  = Field.neighbor d p