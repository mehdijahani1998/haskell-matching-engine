module Ownership where

import Data.Map
import Coverage
import ME

ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership handler rq s = case rq of
  (NewOrderRq o) -> do  
    (rs, s') <- handler rq s
    case status rs of
      Accepted ->  if ownershipPreCheck maxOwnership o s then do
          (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC1"
        else
          (NewOrderRs Rejected [], s) `covers` "OSC2"
      Rejected -> (rs, s') `covers` "OSC3"
  (CancelOrderRq rqid oid side) -> do
    (rs, s') <- handler rq s
    (rs, s') `covers` "OSC4"
  _ -> handler rq s

updateOwnershipInfo :: [Trade] -> MEState -> MEState
updateOwnershipInfo ts (MEState ob ci oi) =
  MEState ob ci (Prelude.foldl updateOwnership oi ts)
   
updateOwnership :: OwnershipInfo -> Trade -> OwnershipInfo
updateOwnership oi t =
  adjust (+ q) bshid $ adjust (\v -> v - q) sshid oi
  where
    q = quantityTraded t
    bshid = buyerShId t
    sshid = sellerShId t  

ownershipPreCheck :: Int -> Order -> MEState -> Bool
ownershipPreCheck maxOwnership o (MEState ob _ ownership) =
  case side o of
    Buy -> (ownership!shi) + (quantity o) + (totalQuantity Buy shi ob) < maxOwnership
    Sell -> (quantity o) + (totalQuantity Sell shi ob) <= (ownership!shi)
  where
    shi = shid o