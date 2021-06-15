module Ownership (ownershipCheck) where

import           Coverage
import           Data.Map
import           ME


ownershipCheckOnAccept :: Int -> Order -> (Response -> Maybe Order) -> Response -> MEState -> Response -> MEState -> Coverage (Response, MEState)
ownershipCheckOnAccept maxOwnership o oldOrderFunc emptyResponse s rs s' = do
    if ownershipPreCheck maxOwnership o (oldOrderFunc rs) s
        then (rs, updateOwnershipInfo (trades rs) s') `covers` "OSC1"
        else (emptyResponse, s) `covers` "OSC2"

ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership handler rq@(NewOrderRq o) s =
    runOnAccept handler rq s "OSC" $ ownershipCheckOnAccept maxOwnership o (const Nothing) $ NewOrderRs Rejected []

ownershipCheck maxOwnership handler rq@(ReplaceOrderRq _ o) s =
    runOnAccept handler rq s "OSC" $ ownershipCheckOnAccept maxOwnership o oldOrder $ ReplaceOrderRs Rejected Nothing []

ownershipCheck _ handler rq s = handler rq s


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
