-- dropPhase.hs
module DropPhase
    (
    land_puyo,
    drop_puyo,
    )
    where

import Control.Applicative
import qualified Control.Monad      as MND

import qualified Typedata   as T (
    AreaPosition,
    PositionY,
    Area(..),
    Direction(DUp,DDown),
    AnimationType(Normal, Dropping),
    UnionCheck(NotYet),
    Power,
    )
import qualified Utility    as U (isPuyo, neighbor_area)
import qualified Variable   as V 
import qualified World      as W (
    animeStartLanding,
    animeStartDropping,
    landingDefauletPower,
    )

import PlayerState
import QueryPS (
    get_fieldStateArea,
    is_neighborSpace,
    get_PlayerPuyoExistent,
    get_PlayerPuyoColors,
    get_PlayerPuyoPosition,
    get_PlayerPuyoDirection,
    )
import OverwritingPS   (
    remove_playerPuyo,
    renew_fieldArea,
    )  

--------------------------------------------------------------------------------
-- 操作ぷよの着地
--------------------------------------------------------------------------------
land_puyo       :: PlayerState -> IO()
land_puyo state =  do
    b <- get_PlayerPuyoExistent state
    MND.when b $ add_playerpuyo >> remove_playerPuyo state
  where
    -- 操作ぷよをフィールドの状態に加える。
    add_playerpuyo  :: IO()
    add_playerpuyo  =  do
        (cb, cm)    <- get_PlayerPuyoColors     state
        pos@(y,_)   <- get_PlayerPuyoPosition   state
        d           <- get_PlayerPuyoDirection  state
        let pos'@(y',_) = U.neighbor_area d pos
        boolb   <- is_neighborSpace pos  T.DDown state
        boolm   <- is_neighborSpace pos' T.DDown state
        let powerM = if d == T.DDown then W.landingDefauletPower - 1
                                     else W.landingDefauletPower
            powerB = if d == T.DUp   then W.landingDefauletPower - 1
                                     else W.landingDefauletPower
            atb = if boolb && d /= T.DDown  then T.Normal
                                            else W.animeStartLanding powerB
            atm = if boolm && d /= T.DUp    then T.Normal
                                            else W.animeStartLanding powerM
        MND.when (y  >= V.hidingFieldRank)
                 $ renew_fieldArea state pos  $ T.Puyo cb T.NotYet atb
        MND.when (y' >= V.hidingFieldRank)
                 $ renew_fieldArea state pos' $ T.Puyo cm T.NotYet atm
        
        areaB   <- get_fieldStateArea (U.neighbor_area T.DDown pos ) state
        areaM   <- get_fieldStateArea (U.neighbor_area T.DDown pos') state
        
        transmitAnimeLanding powerB areaB (U.neighbor_area T.DDown pos ) state
        transmitAnimeLanding powerM areaM (U.neighbor_area T.DDown pos') state
  
--------------------------------------------------------------------------------
-- ちぎり落下
--------------------------------------------------------------------------------
drop_puyo           :: V.GameState -> PlayerState -> IO Bool
drop_puyo gs state  =
    MND.foldM fff False fieldArray_indices
  where
    -- ちぎり落下の適用範囲。
    fieldArray_indices
        =[(y, x) | x <- [2..(V.fieldSizeX' gs - 1)],
                   let sizeY = V.fieldSizeY' gs,
                   y <- [(sizeY - 1), (sizeY - 2) .. V.hidingFieldRank] ]
    fff     :: Bool -> T.AreaPosition -> IO Bool
    fff b p =  do
        b'  <- drop_areaPuyo  p
        return $ b || b'
      where    
        -- １つのぷよを１エリア分落下させる。接地ぷよからの高さを返す。
        drop_areaPuyo   :: T.AreaPosition -> IO Bool
        drop_areaPuyo p =  do
            areaObj    <- get_fieldStateArea p state 
            flagSpace  <- is_neighborSpace p T.DDown state
            
            if flagSpace && U.isPuyo areaObj 
              then do 
                renew_fieldArea state p' areaObj
                renew_fieldArea state p  T.Space
                flagSpace'  <- is_neighborSpace p' T.DDown state
                areaObj''   <- get_fieldStateArea p'' state
                if flagSpace' || maching areaObj''
                  then do
                    rewrite_animationType state p' areaObj W.animeStartDropping
                  else do
                    rewrite_animationType state p' areaObj defaultLanding
                    transmitDefault areaObj'' p'' state   
                return True
              else do
                return False
          where
            p'@(y', _)  = U.neighbor_area T.DDown p
            p'' = U.neighbor_area T.DDown p'
            maching (T.Puyo  _ _ (T.Dropping _ ))   = True
            maching (T.Ojama   _ (T.Dropping _ ))   = True
            maching _                               = False

--------------------------------------------------------------------------------
-- アニメーションタイプ
--------------------------------------------------------------------------------
-- アニメーションタイプを書き換える。
rewrite_animationType   :: PlayerState -> T.AreaPosition 
                        -> T.Area -> T.AnimationType -> IO()
rewrite_animationType state p (T.Puyo c u _) at
    =  renew_fieldArea state p $ T.Puyo c u at
rewrite_animationType state p (T.Ojama  u _) at
    =  renew_fieldArea state p $ T.Ojama u at
rewrite_animationType _ _ _ _ 
    =  return ()

-- 落下があった列のぷよをアニメーションさせる。
transmitAnimeLanding    :: T.Power -> T.Area -> T.AreaPosition -> PlayerState 
                        -> IO()
transmitAnimeLanding 0 _                          _ _     = return ()
transmitAnimeLanding a area@(T.Puyo _ _ T.Normal) p state = tAL a area p state
transmitAnimeLanding a area@(T.Ojama  _ T.Normal) p state = tAL a area p state
transmitAnimeLanding _ _                          _ _     = return ()
    
tAL a area p state = do
    rewrite_animationType state p area $ W.animeStartLanding (a - 1)
    area'   <- get_fieldStateArea p' state
    transmitAnimeLanding (a - 1) area' p' state
  where
    p' = U.neighbor_area T.DDown p

-- transmitAnimeLandingの標準化
transmitDefault :: T.Area -> T.AreaPosition -> PlayerState -> IO()
transmitDefault a p s = transmitAnimeLanding W.landingDefauletPower a p s

defaultLanding  = W.animeStartLanding W.landingDefauletPower