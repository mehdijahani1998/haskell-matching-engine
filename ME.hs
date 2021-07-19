module ME
    ( Order (..)
    , Quantity
    , Price
    , TimeStamp
    , OrderID
    , BrokerID
    , ShareholderID
    , CreditInfo
    , OwnershipInfo
    , Side (..)
    , OrderQueue
    , OrderBook (..)
    , Trade (..)
    , MEState (..)
    , Request (..)
    , Response (..)
    , ResponseStatus (..)
    , initMEState
    , limitOrder
    , icebergOrder
    , removeOrderFromOrderBook
    , valueTraded
    , matchNewOrder
    , cancelOrder
    , adjustPeakSizeOnReplace
    , shouldReplaceInPlace
    , replaceOrderInPlace
    , queueBySide
    , sameSideQueue
    , reject
    ) where

import           Control.Exception (assert)
import           Coverage
import qualified Data.List         as List
import qualified Data.Map          as Map


type Quantity = Int
type Price = Int
type TimeStamp = Int
type OrderID = Int
type BrokerID = Int
type ShareholderID = Int
type CreditInfo = Map.Map BrokerID Int
type OwnershipInfo = Map.Map ShareholderID Int

data Side = Buy | Sell deriving (Show, Eq, Ord)

data Order = LimitOrder
    { oid         :: OrderID
    , brid        :: BrokerID
    , shid        :: ShareholderID
    , price       :: Price
    , quantity    :: Quantity
    , side        :: Side
    , minQty      :: Maybe Quantity
    , fillAndKill :: Bool
    } | IcebergOrder
    { oid          :: OrderID
    , brid         :: BrokerID
    , shid         :: ShareholderID
    , price        :: Price
    , quantity     :: Quantity
    , side         :: Side
    , minQty       :: Maybe Quantity
    , fillAndKill  :: Bool
    , disclosedQty :: Quantity
    , visibleQty   :: Quantity
    } deriving (Show, Eq)

type OrderQueue = [Order]

data OrderBook = OrderBook
    { buyQueue  :: OrderQueue
    , sellQueue :: OrderQueue
    } deriving (Show, Eq)


data Trade = Trade
  { priceTraded      :: Price
    , quantityTraded :: Quantity
    , buyId          :: OrderID
    , sellId         :: OrderID
    , buyerShId      :: ShareholderID
    , buyerBrId      :: BrokerID
    , sellerShId     :: ShareholderID
    , sellerBrId     :: BrokerID
    } deriving (Show, Eq)


data MEState = MEState
    { orderBook      :: OrderBook
    , creditInfo     :: CreditInfo
    , ownershipInfo  :: OwnershipInfo
    , referencePrice :: Price
    } deriving (Show, Eq)


initMEState :: MEState
initMEState = MEState (OrderBook [] []) Map.empty Map.empty 0


data Request = NewOrderRq
    { order :: Order
    } | CancelOrderRq
    { rqId    :: OrderID
    , oldOid  :: OrderID
    , oldSide :: Side
    } | ReplaceOrderRq
    { oldOid :: OrderID
    , order  :: Order
    } | SetCreditRq
    { broker :: BrokerID
    , credit :: Int
    } | SetOwnershipRq
    { shareholder :: ShareholderID
    , shares      :: Int
    } | SetReferencePriceRq
    { newReferencePrice :: Int
    } deriving (Show, Eq)


data ResponseStatus = Accepted | Rejected deriving (Show, Eq)

accepted :: ResponseStatus -> Bool
accepted Accepted = True

accepted Rejected = False


data Response = NewOrderRs
    { status :: ResponseStatus
    , trades :: [Trade]
    , state :: MEState
    } | CancelOrderRs
    { status   :: ResponseStatus
    , oldOrder :: Maybe Order
    , state :: MEState
    } | ReplaceOrderRs
    { status   :: ResponseStatus
    , oldOrder :: Maybe Order
    , trades   :: [Trade]
    , state :: MEState
    } | SetCreditRs
    { status :: ResponseStatus
    , state :: MEState
    } | SetOwnershipRs
    { status :: ResponseStatus
    , state :: MEState
    } | SetReferencePriceRs
    { status :: ResponseStatus
    , state :: MEState
    } deriving (Show, Eq)


rejectedNewOrderRs :: MEState -> Response
rejectedNewOrderRs = NewOrderRs Rejected []


rejectedReplaceOrderRs :: MEState -> Response
rejectedReplaceOrderRs = ReplaceOrderRs Rejected Nothing []


rejectedCancelOrderRs :: MEState -> Response
rejectedCancelOrderRs = CancelOrderRs Rejected Nothing


rejectedSetCreditRs :: MEState -> Response
rejectedSetCreditRs = SetCreditRs Rejected


rejectedSetOwnershipRs :: MEState -> Response
rejectedSetOwnershipRs = SetOwnershipRs Rejected


rejectedSetReferencePriceRs :: MEState -> Response
rejectedSetReferencePriceRs = SetReferencePriceRs Rejected


reject :: Request -> MEState -> Response
reject NewOrderRq {} = rejectedNewOrderRs

reject ReplaceOrderRq {} = rejectedReplaceOrderRs

reject CancelOrderRq {} = rejectedCancelOrderRs

reject SetCreditRq {} = rejectedSetCreditRs

reject SetOwnershipRq {} = rejectedSetOwnershipRs

reject SetReferencePriceRq {} = rejectedSetReferencePriceRs


valueTraded :: Trade -> Int
valueTraded t = (priceTraded t) * (quantityTraded t)


limitOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Order
limitOrder i bi shi p q s m fak =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of
        { (Just mq) -> assert (mq > 0)
        ; otherwise -> id
        } $
    LimitOrder i bi shi p q s m fak


icebergOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Quantity -> Quantity -> Order
icebergOrder i bi shi p q s m fak dq vq =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of
        { (Just mq) -> assert (mq > 0)
        ; otherwise -> id
        } $
    assert (dq <= q) $
    assert (vq > 0 && vq <= dq && vq <= q) $
    IcebergOrder i bi shi p q s m fak dq vq


isIceberg :: Order -> Bool
isIceberg IcebergOrder {} = True

isIceberg LimitOrder {}   = False


displayedQty :: Order -> Quantity
displayedQty o@IcebergOrder {} = visibleQty o

displayedQty o@LimitOrder {}   = quantity o


decQty :: Order -> Quantity -> Order
decQty o@(LimitOrder _ _ _ _ q _ _ _) q'        = setQty o $ q - q'

decQty o@(IcebergOrder _ _ _ _ q _ _ _ _ vq) q' = setQties o (q - q') (vq - q')


setQty :: Order -> Quantity -> Order
setQty (LimitOrder i bi shi p _ s m fak) q' =
    limitOrder i bi shi p q' s m fak

setQty (IcebergOrder i bi shi p _ s m fak dq vq) q' =
    icebergOrder i bi shi p q' s m fak dq vq


setVisibleQty :: Order -> Quantity -> Order
setVisibleQty (IcebergOrder i bi shi p q s m fak dq _) vq' =
    icebergOrder i bi shi p q s m fak dq vq'


setQties :: Order -> Quantity -> Quantity -> Order
setQties (IcebergOrder i bi shi p _ s m fak dq _) q' vq' =
    icebergOrder i bi shi p q' s m fak dq vq'


removeOrderFromQueue :: Order -> OrderQueue -> OrderQueue
removeOrderFromQueue = List.deleteBy (\ o1 o2 -> oid o1 == oid o2)


replaceOrderInQueue :: OrderID -> Order -> OrderQueue -> OrderQueue
replaceOrderInQueue _ _ [] = []

replaceOrderInQueue ooid o (h:t)
    | oid h == ooid = o:t
    | otherwise     = h:t


findOrderFromQueueByID :: OrderID -> OrderQueue -> Maybe Order
findOrderFromQueueByID oidToFind oq
    | null filtered = Nothing
    | otherwise     = Just $ head filtered
  where
    filtered = List.filter (\o -> oid o == oidToFind) oq


applyOnQueue :: (OrderQueue -> OrderQueue) -> Side -> OrderBook -> OrderBook
applyOnQueue f side (OrderBook bq sq)
    | side == Buy  = OrderBook (f bq) sq
    | side == Sell = OrderBook bq (f sq)
    | otherwise = error "invalid Side"


applyOnSameSideQueue :: (OrderQueue -> OrderQueue) -> Order -> OrderBook -> OrderBook
applyOnSameSideQueue f o = applyOnQueue f (side o)


queueBySide :: Side -> OrderBook -> OrderQueue
queueBySide side ob
    | side == Buy  = buyQueue ob
    | side == Sell = sellQueue ob
    | otherwise = error "invalid Side"


sameSideQueue :: Order -> OrderBook -> OrderQueue
sameSideQueue o = queueBySide $ side o


oppositeSideQueue :: Order -> OrderBook -> OrderQueue
oppositeSideQueue o = queueBySide os
  where
    os = case side o of
        Buy  -> Sell
        Sell -> Buy


removeOrderFromOrderBook :: Order -> OrderBook -> OrderBook
removeOrderFromOrderBook o = applyOnSameSideQueue (removeOrderFromQueue o) o


replaceOrderInOrderBook :: OrderID -> Order -> OrderBook -> OrderBook
replaceOrderInOrderBook ooid o = applyOnSameSideQueue (replaceOrderInQueue ooid o) o


findOrderFromOrderBookByID :: OrderID -> Side -> OrderBook ->  Maybe Order
findOrderFromOrderBookByID oid side ob = findOrderFromQueueByID oid $ queueBySide side ob


queuesBefore :: Order -> Order -> Bool
queuesBefore o o'
    | side o == Sell && side o' == Sell = price o < price o'
    | side o == Buy  && side o' == Buy  = price o > price o'
    | otherwise = error "incomparable orders"


enqueueOrder :: Order -> OrderQueue -> OrderQueue
enqueueOrder o@LimitOrder {} = enqueueOrder' o

enqueueOrder o@(IcebergOrder _ _ _ _ q _ _ _ dq _) = enqueueOrder' $ setVisibleQty o $ min q dq


enqueueOrder' :: Order -> OrderQueue -> OrderQueue
enqueueOrder' o [] = [o]

enqueueOrder' o (o1:os)
    | queuesBefore o o1 = o:(o1:os)
    | otherwise = o1:enqueueOrder' o os


enqueue :: Maybe Order -> OrderBook -> OrderBook
enqueue Nothing {} = id

enqueue (Just o)   = applyOnSameSideQueue (enqueueOrder o) o


enqueueRemainder :: OrderQueue -> Order -> Coverage OrderQueue
enqueueRemainder os o@LimitOrder {}
    | q == 0 = os `covers` "ELR-1"
    | otherwise = enqueueOrder o os `covers` "ELR-2"
  where
    q = quantity o

enqueueRemainder os o@IcebergOrder {}
    | q == 0 = os `covers` "EIR-1"
    | vq == 0 && q <= dq = enqueueOrder (setVisibleQty o q) os `covers` "EIR-2"
    | vq == 0 && q > dq = enqueueOrder (setVisibleQty o dq) os `covers` "EIR-3"
    | otherwise = enqueueOrder o os `covers` "EIR-4"
  where
    q = quantity o
    vq = visibleQty o
    dq = disclosedQty o


canBeMatchedWithOppositeQueueHead :: Order -> Order -> Bool
canBeMatchedWithOppositeQueueHead o h
    | s == Buy  = newp >= headp
    | s == Sell = newp <= headp
  where
    s = side o
    newp = price o
    headp = price h

trade :: Price -> Quantity -> Order -> Order -> Trade
trade p q newo oppositeo
    | side newo == Buy  = Trade p q newi headi newshi newbi headshi headbi
    | side newo == Sell = Trade p q headi newi headshi headbi newshi newbi
  where
    newi = oid newo
    newshi = shid newo
    newbi = brid newo
    headi = oid oppositeo
    headshi = shid oppositeo
    headbi = brid oppositeo


match :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
match o [] = (Just o, [], []) `covers` "M-0"

match o oq@(h:os)
    | not $ canBeMatchedWithOppositeQueueHead o h = (Just o, oq, []) `covers` "M-1"
    | newq < headq = (Nothing, (decQty h newq):os, [trade headp newq o h]) `covers` "M-2"
    | newq == headq = do
        newQueue <- enqueueRemainder os $ decQty h newq
        (Nothing, newQueue, [trade headp newq o h]) `covers` "M-3"
    | newq > headq = do
        newQueue <- enqueueRemainder os $ decQty h headq
        (o', ob', ts') <- match (decQty o headq) newQueue
        (o', ob', (trade headp headq o h):ts') `covers` "M-4"
  where
    newq = quantity o
    headp = price h
    headq = displayedQty h


updateOppositeQueueInBook :: Order -> OrderQueue -> OrderBook -> OrderBook
updateOppositeQueueInBook o oq ob
    | side o == Buy  = ob{sellQueue = oq}
    | side o == Sell = ob{buyQueue = oq}
    | otherwise = error "invalid Side"


matchNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
matchNewOrder o ob = do
    let oq = oppositeSideQueue o ob
    (remo, oq', ts) <- match o oq
    let ob' = updateOppositeQueueInBook o oq' ob
    (enqueue remo ob', ts) `covers` "MNO"


cancelOrder :: OrderID -> Side -> OrderBook -> Coverage (OrderBook, Maybe Order)
cancelOrder oid side ob = do
    case findOrderFromOrderBookByID oid side ob of
        Just o -> (ob', Just o) `covers` "CO-1"
          where
            ob' = removeOrderFromOrderBook o ob
        Nothing -> (ob, Nothing) `covers` "CO-2"


replaceOrderInPlace :: OrderID -> Order -> OrderBook -> Coverage (OrderBook, [Trade])
replaceOrderInPlace ooid o ob = (replaceOrderInOrderBook ooid o ob, []) `covers` "ROIP-1"


shouldReplaceInPlace :: Order -> Order -> Bool
shouldReplaceInPlace oldOrder order
    | displayedQty order > displayedQty oldOrder = False
    | price order /= price oldOrder = False
    | otherwise = True


adjustPeakSizeOnReplace :: Order -> Order -> Order
adjustPeakSizeOnReplace oldOrder@LimitOrder {} notAdjustedNewOrder = notAdjustedNewOrder

adjustPeakSizeOnReplace oldOrder@IcebergOrder {} notAdjustedNewOrder@LimitOrder {} = notAdjustedNewOrder

adjustPeakSizeOnReplace oldOrder@IcebergOrder {} notAdjustedNewOrder@IcebergOrder {}
    | oldvq == olddq = setVisibleQty notAdjustedNewOrder newdq
    | oldvq < olddq && oldvq > newdq = setVisibleQty notAdjustedNewOrder newdq
    | otherwise = notAdjustedNewOrder
  where
    olddq = disclosedQty oldOrder
    newdq = disclosedQty notAdjustedNewOrder
    oldvq = visibleQty oldOrder
