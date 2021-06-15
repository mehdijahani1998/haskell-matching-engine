module FillAndKill (fillAndKillProc) where

import           Coverage
import           ME


removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState o s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }


fillAndKillProc :: Decorator
fillAndKillProc handler rq@(NewOrderRq o) s =
    runOnAccept handler rq s "FKP" $ \ s rs s' -> if fillAndKill o
        then (rs, removeOrderFromState o s') `covers` "FKP1"
        else (rs, s') `covers` "FKP2"

fillAndKillProc handler rq s = handler rq s
