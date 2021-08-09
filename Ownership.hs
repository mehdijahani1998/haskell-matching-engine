module Ownership (ownershipCheck) where

import           Coverage
import           Data.Map
import           Decorator
import           ME


ownershipCheck :: Float -> Decorator
ownershipCheck maxOwnershipPortion =
    decorateOnAccept "OSC" $ ownershipCheckByType maxOwnershipPortion


ownershipCheckByType :: Float -> PartialDecorator
ownershipCheckByType maxOwnershipPortion rq@NewOrderRq {} s rs =
    ownershipCheckForArrivingOrder maxOwnershipPortion rq s rs

ownershipCheckByType maxOwnershipPortion rq@ReplaceOrderRq {} s rs =
    ownershipCheckForArrivingOrder maxOwnershipPortion rq s rs

ownershipCheckByType _ _ _ rs =
    rs `covers` "OSC-P"


getOldOrder :: Response -> Maybe Order
getOldOrder rs@ReplaceOrderRs {} =
    oldOrder rs

getOldOrder rs@CancelOrderRs {} =
    oldOrder rs

getOldOrder _ =
    Nothing


ownershipCheckForArrivingOrder :: Float -> PartialDecorator
ownershipCheckForArrivingOrder maxOwnershipPortion rq s rs = do
    let o = order rq
    let oldo = getOldOrder rs
    let s' = state rs
    if ownershipPreCheck maxOwnershipPortion o oldo s
        then rs { state = updateOwnershipInfo (trades rs) s' } `covers` "OSC1"
        else reject rq s `covers` "OSC2"


updateOwnershipInfo :: [Trade] -> MEState -> MEState
updateOwnershipInfo ts state =
    state {ownershipInfo = Prelude.foldl updateOwnership oi ts}
  where
    oi = ownershipInfo state


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


ownershipPreCheck :: Float -> Order -> Maybe Order -> MEState -> Bool
ownershipPreCheck maxOwnershipPortion o oldOrder state = do
    shi `member` ownership && case side o of
        Buy  -> (ownership!shi) + (quantity o) + (totalQuantity Buy shi ob) - (quantityInBook oldOrder ob) < maxOwnership
        Sell -> (quantity o) + (totalQuantity Sell shi ob) - (quantityInBook oldOrder ob) <= (ownership!shi)
  where
    shi = shid o
    ownership = ownershipInfo state
    ob = orderBook state
    shares = totalShares state
    maxOwnership = floor $ fromIntegral shares * maxOwnershipPortion


totalQuantity :: Side -> ShareholderID -> OrderBook -> Quantity
totalQuantity side shi ob =
    sum $
    Prelude.map quantity $
    Prelude.filter (\o -> shid o == shi) $
    queueBySide side ob
