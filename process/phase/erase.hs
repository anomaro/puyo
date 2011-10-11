-- erasePhase.hs
module Process.Phase.Erase
    (
    erase_puyo,
    rewriteSpase_puyo,
    )
    where

import qualified Control.Monad      as MND

import qualified Common.PlayerIdentity  as Identity (against, territory)
import qualified Common.Area            as Area
import qualified Common.Direction       as Direction
import qualified Common.Score           as Score
import Common.Color (Color)
import qualified Common.Field           as Field

import qualified Common.DataType   as T
import qualified Common.Function    as U
import qualified State.Setting  as V
import qualified Common.Name      as W

import State.Player.DataType
import State.Player.Query
import State.Player.Overwriting

import Standardizable

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- 結合状態を調べてぷよを消滅させる。ぷよが消えた場合True
erase_puyo          :: V.GameState -> PlayerState -> IO Bool
erase_puyo gs state =  do
    renewScore Score.expandChain state
    MND.foldM f False $ Field.arrayIndices gs
  where
    f :: Bool -> Field.Position -> IO Bool
    f b p = do
        area  <- get_fieldStateArea p state 
        numUnion    <- if Area.isUnionCheck Nothing area p
                         then check_union state p
                         else return 0
        bool    <- if numUnion < V.get V.ErasePuyo gs
                 then return False
                 else do
                area <- get_fieldStateArea p state
                erase_unionPuyo state p
                renewScore (Score.expandUniCol numUnion (Area.color area)) state
                return True
        off_unionCheck state p
        return $ b || bool

-- 結合状態を調べてぷよを消滅させる。（消滅フラグが立っているエリアを空白にする）
rewriteSpase_puyo           :: V.GameState -> PlayerState -> IO ()
rewriteSpase_puyo gs state  =  do
    MND.mapM_ f $ Field.arrayIndices gs
    renewScore Score.calculate state
    calculateYokoku
  where
    f :: Field.Position -> IO()
    f p = do
        area    <- get_fieldStateArea p state
        MND.when (Area.isReplacedSpace area p)
                 $ renew_fieldArea state p standard
    -- 予告ぷよを算出する。
    calculateYokoku =  do
        score <- get_score state
        let (n, newScore)  = Score.calculateYokoku score (V.get V.OjamaRate gs)
        renewYokoku id id (n +) trt state
        renewScore (const newScore) state
      where
        trt = Identity.against . Identity.territory $ get_playerIdentity state

--------------------------------------------------------------------------------
-- 結合チェック状態を未調査状態にする。
off_unionCheck :: PlayerState -> Field.Position -> IO()
off_unionCheck state p = do
    area <- get_fieldStateArea p state
    MND.when (Area.isUnionCheckFinished Nothing area p)
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
        MND.when (Area.isEraseOjamaPuyo area)   $ eraseOjamaPuyo  state p'
        MND.when (Area.isUnionCheckFinished (Just color) area p') 
                                                $ erase_unionPuyo state p'
      where
        p'  = Field.neighbor d p
        -- おじゃまぷよを消滅させる。（消滅フラグを立てる。）
        eraseOjamaPuyo      :: PlayerState -> Field.Position -> IO()
        eraseOjamaPuyo s p  =  renew_fieldArea s p $ Area.eracingPuyo Nothing

-- エリア対象の結合チェック。このエリアがチェック対象かどうかは事前に調べてある。
check_union :: PlayerState -> Field.Position -> IO T.NumOfUnion
check_union state p    = do
    area <- get_fieldStateArea p state
    renew_fieldArea state p $ Area.unionCheckCompletion area
    MND.foldM (fff $ Area.color area) 1 Direction.areas
  where
    fff :: Color -> T.NumOfUnion -> Direction.Area -> IO T.NumOfUnion
    fff color n d = do
        area    <- get_fieldStateArea p' state
        if Area.isUnionCheck (Just color) area p'
          then check_union state p' >>= return . (+ n)
          else return n
      where
        p'  = Field.neighbor d p