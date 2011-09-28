module Render
( render
) where

import qualified RenderGame     as R
import qualified Stage          as S
import qualified Configuration  as C

--------------------------------------------------------------------------------
--  •`‰æ
--------------------------------------------------------------------------------
render  :: S.GameStage -> IO()
render (S.Configuration gs sl _)            = C.renderConfiguration gs sl
render (S.Game _ gs (state1P, state2P) gdc) =
        R.render_gameField gs state1P gdc >> R.render_gameField gs state2P gdc