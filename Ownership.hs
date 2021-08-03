module Ownership (ownershipCheck) where

import           Coverage
import           Data.Map
import           Decorator
import           ME


ownershipCheck :: Int -> Decorator
ownershipCheck maxOwnership =
    decorateOnAccept "OSC" $ ownershipCheckByType maxOwnership


ownershipCheckByType :: Int -> PartialDecorator
ownershipCheckByType maxOwnership rq@NewOrderRq {} s rs =
    ownershipCheckForArrivingOrder maxOwnership rq s rs

ownershipCheckByType maxOwnership rq@ReplaceOrderRq {} s rs =
    ownershipCheckForArrivingOrder maxOwnership rq s rs

ownershipCheckByType _ _ _ rs =
    rs `covers` "OSC-P"


getOldOrder :: Response -> Maybe Order
getOldOrder rs@ReplaceOrderRs {} =
    oldOrder rs

getOldOrder rs@CancelOrderRs {} =
    oldOrder rs

getOldOrder _ =
    Nothing


ownershipCheckForArrivingOrder :: Int -> PartialDecorator
ownershipCheckForArrivingOrder maxOwnership rq s rs = do
    let o = order rq
    let oldo = getOldOrder rs
    let s' = state rs
    if ownershipPreCheck maxOwnership o oldo s
        then rs { state = updateOwnershipInfo (trades rs) s' } `covers` "OSC1"
        else reject rq s `covers` "OSC2"


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
