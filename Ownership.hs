module Ownership (ownershipCheck) where

import           Coverage
import           Data.Map
import           ME


ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership =
    decorateOnAccept "OSC" $ ownershipCheckByType maxOwnership


ownershipCheckByType :: Int -> PartialDecorator
ownershipCheckByType maxOwnership (NewOrderRq o) s rs s' =
    if ownershipPreCheck maxOwnership o Nothing s
        then (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC1"
        else (NewOrderRs Rejected [], s) `covers` "OSC2"

ownershipCheckByType maxOwnership (ReplaceOrderRq _ o) s rs s' =
    if ownershipPreCheck maxOwnership o (oldOrder rs) s
        then (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC3"
        else (ReplaceOrderRs Rejected Nothing [], s) `covers` "OSC4"

ownershipCheckByType _ _ _ rs s' =
    (rs, s') `covers` "OSC5"


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
    Buy  -> (ownership!shi) + (quantity o) + (totalQuantity Buy shi ob) - (quantityInBook oldOrder ob) < maxOwnership
    Sell -> (quantity o) + (totalQuantity Sell shi ob) - (quantityInBook oldOrder ob) <= (ownership!shi)
  where
    shi = shid o


totalQuantity :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantity side shi ob =
    sum $
    Prelude.map quantity $
    Prelude.filter (\o -> shid o == shi) $
    queueBySide side ob
