module Ownership where

import Data.Map
import Coverage
import ME

ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership handler rq s = case rq of
  (NewOrderRq o) -> do  
    (rs, s') <- handler rq s
    case status rs of
      Accepted ->  if ownershipPreCheck maxOwnership o Nothing s then do
          (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC1"
        else
          (NewOrderRs Rejected [], s) `covers` "OSC2"
      Rejected -> (rs, s') `covers` "OSC3"
  (CancelOrderRq rqid oid side) -> do
    (rs, s') <- handler rq s
    (rs, s') `covers` "OSC4"
  (ReplaceOrderRq oldoid o) -> do
    (rs, s') <- handler rq s
    case status rs of
      Accepted ->  if ownershipPreCheck maxOwnership o (oldOrder rs) s then do
          (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC5"
        else
          (ReplaceOrderRs Rejected Nothing [], s) `covers` "OSC6"
      Rejected -> (rs, s') `covers` "OSC7"
  _ -> handler rq s

updateOwnershipInfo :: [Trade] -> MEState -> MEState
updateOwnershipInfo ts (MEState ob ci oi) =
  MEState ob ci (Prelude.foldl updateOwnership oi ts)
   
updateOwnership :: OwnershipInfo -> Trade -> OwnershipInfo
updateOwnership oi t =
  adjust (+ q) bshid $ adjust (subtract q) sshid oi
  where
    q = quantityTraded t
    bshid = buyerShId t
    sshid = sellerShId t

quantityInQueue :: Maybe Order -> OrderBook -> Int
quantityInQueue (Just o) ob =
  sum $ 
  Prelude.map quantity $
  Prelude.filter (\orderInQueue -> oid orderInQueue == oid o) $
  (if side o == Buy then buyQueue else sellQueue) $
  ob

quantityInQueue Nothing ob = 0

ownershipPreCheck :: Int -> Order -> Maybe Order -> MEState -> Bool
ownershipPreCheck maxOwnership o oldOrder (MEState ob _ ownership) =
  case side o of
    Buy -> (ownership!shi) + (quantity o) + (totalQuantity Buy shi ob) - (quantityInQueue oldOrder ob) < maxOwnership
    Sell -> (quantity o) + (totalQuantity Sell shi ob) - (quantityInQueue oldOrder ob) <= (ownership!shi)
  where
    shi = shid o

totalQuantity :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantity side shi ob =
  sum $
  Prelude.map quantity $
  Prelude.filter (\o -> shid o == shi) $
  queue side ob
