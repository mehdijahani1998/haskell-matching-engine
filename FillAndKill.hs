module FillAndKill where

import Data.Map
import Coverage
import ME

removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState o s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }

fillAndKillProc :: Decorator
fillAndKillProc handler =
  \rq s ->
    case rq of
      (NewOrderRq o) -> do
        { (rs, s') <- handler rq s
        ; if fillAndKill o then
            (rs, removeOrderFromState o s') `covers` "FKP1"
          else
            (rs, s') `covers` "FKP2"
        }
      _ -> handler rq s

