module Ownership (ownershipCheck) where

import           Coverage
import           Data.Map
import           ME


ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership handler rq s = case rq of
    (NewOrderRq o) -> do
        (rs, s') <- handler rq s
        case status rs of
            Accepted ->  if ownershipPreCheck maxOwnership o Nothing s
                then (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC1"
                else (NewOrderRs Rejected [], s) `covers` "OSC2"
            Rejected -> (rs, s') `covers` "OSC3"
    (CancelOrderRq rqid oid side) -> do
        (rs, s') <- handler rq s
        (rs, s') `covers` "OSC4"
    (ReplaceOrderRq oldoid o) -> do
        (rs, s') <- handler rq s
        case status rs of
            Accepted ->  if ownershipPreCheck maxOwnership o (oldOrder rs) s
                then (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC5"
                else (ReplaceOrderRs Rejected Nothing [], s) `covers` "OSC6"
            Rejected -> (rs, s') `covers` "OSC7"
    _ -> handler rq s


updateOwnershipInfo :: [Trade] -> MEState -> MEState
updateOwnershipInfo ts (MEState ob ci oi rp) =
  MEState ob ci (Prelude.foldl updateOwnership oi ts) rp


updateOwnership :: OwnershipInfo -> Trade -> OwnershipInfo
updateOwnership oi t =
    adjust (+ q) bshid $ adjust (subtract q) sshid oi
  where
    q = quantityTraded t
    bshid = buyerShId t
    sshid = sellerShId t


quantityInBook :: Maybe Order -> OrderBook -> Int
quantityInBook (Just o) ob =
    quantityInQueue o $ sameSideQueue o ob

quantityInBook Nothing _ = 0


quantityInQueue :: Order -> OrderQueue -> Int
quantityInQueue o q =
    sum $
    Prelude.map quantity $
    Prelude.filter (\orderInQueue -> oid orderInQueue == oid o) $
    q

ownershipPreCheck :: Int -> Order -> Maybe Order -> MEState -> Bool
ownershipPreCheck maxOwnership o oldOrder (MEState ob _ ownership _) = case side o of
    Buy -> (ownership!shi) + (quantity o) + (totalQuantity Buy shi ob) - (quantityInBook oldOrder ob) < maxOwnership
    Sell -> (quantity o) + (totalQuantity Sell shi ob) - (quantityInBook oldOrder ob) <= (ownership!shi)
  where
    shi = shid o


totalQuantity :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantity side shi ob =
    sum $
    Prelude.map quantity $
    Prelude.filter (\o -> shid o == shi) $
    queueBySide side ob
