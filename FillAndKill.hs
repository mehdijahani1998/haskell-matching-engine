module FillAndKill (fillAndKillProc) where

import           Coverage
import           ME


removeOrderFromState :: Order -> MEState -> MEState
removeOrderFromState o s =
  s { orderBook = removeOrderFromOrderBook o (orderBook s) }


fillAndKillProc :: Decorator
fillAndKillProc handler rq s = do
    (rs, s') <- handler rq s
    case status rs of
        Accepted -> case rq of
            (NewOrderRq o) -> do
                if fillAndKill o
                    then (rs, removeOrderFromState o s') `covers` "FKP1"
                    else (rs, s') `covers` "FKP2"
            _ -> (rs, s') `covers` "FKP3"
        Rejected -> (rs, s') `covers`  "FKP4"
