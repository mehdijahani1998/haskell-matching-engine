module FillAndKill (fillAndKillProc) where

import           Coverage
import           Decorator
import           ME


removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState o s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }


fillAndKillProc :: Decorator
fillAndKillProc =
    decorateOnAccept "FKP" fillAndKillProcByType


fillAndKillProcByType :: PartialDecorator
fillAndKillProcByType  (NewOrderRq o) _ rs s' =
    if fillAndKill o
        then (rs, removeOrderFromState o s') `covers` "FKP1"
        else (rs, s') `covers` "FKP2"

fillAndKillProcByType _ _ rs s' =
    (rs, s') `covers` "FKP3"
