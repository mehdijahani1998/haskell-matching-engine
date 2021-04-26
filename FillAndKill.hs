module FillAndKill where

import Coverage
import ME

removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState o s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }

fillAndKillProc :: Decorator
fillAndKillProc handler rq s = case rq of
  (NewOrderRq o) -> do
    (rs, s') <- handler rq s
    case status rs of
      Accepted -> 
        if fillAndKill o then
          (rs, removeOrderFromState o s') `covers` "FKP1"
        else
          (rs, s') `covers` "FKP2"
      Rejected -> (rs, s') `covers`  "FKP3"
  _ -> handler rq s

