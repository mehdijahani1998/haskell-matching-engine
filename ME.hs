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
    , OrderResponseStatus (..)
    , Handler
    , Decorator
    , initMEState
    , limitOrder
    , icebergOrder
    , removeOrderFromOrderBook
    , valueTraded
    , queue
    , matchNewOrder
    , cancelOrder
    , adjustPeakSizeOnReplace
    , shouldReplaceInPlace
    , replaceOrderInPlace
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


queue :: Side -> OrderBook -> OrderQueue
queue Buy ob  = buyQueue ob
queue Sell ob = sellQueue ob


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

data OrderResponseStatus = Accepted | Rejected deriving (Show, Eq)

data Response = NewOrderRs
    { status :: OrderResponseStatus
    , trades :: [Trade]
    } | CancelOrderRs
    { status   :: OrderResponseStatus
    , oldOrder :: Maybe Order
    } | ReplaceOrderRs
    { status   :: OrderResponseStatus
    , oldOrder :: Maybe Order
    , trades   :: [Trade]
    } | SetCreditRs
    { success :: Bool
    } | SetOwnershipRs
    { success :: Bool
    } | SetReferencePriceRs
    { success :: Bool
    } deriving (Show, Eq)


type Handler = Request -> MEState -> Coverage (Response, MEState)
type Decorator = Handler -> Handler


valueTraded :: Trade -> Int
valueTraded t = (priceTraded t) * (quantityTraded t)


limitOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Order
limitOrder i bi shi p q s m fak =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
    LimitOrder i bi shi p q s m fak


icebergOrder :: OrderID -> BrokerID -> ShareholderID -> Price -> Quantity -> Side -> Maybe Quantity -> Bool -> Quantity -> Quantity -> Order
icebergOrder i bi shi p q s m fak dq vq =
    assert (i >= 0) $
    assert (p > 0) $
    assert (q > 0) $
    case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
    assert (dq <= q) $
    assert (vq > 0 && vq <= dq && vq <= q) $
    IcebergOrder i bi shi p q s m fak dq vq


isIceberg :: Order -> Bool
isIceberg IcebergOrder {} = True

isIceberg LimitOrder {}   = False


displayedQty :: Order -> Quantity
displayedQty order@IcebergOrder {} = visibleQty order

displayedQty order@LimitOrder {}   = quantity order


decQty :: Order -> Quantity -> Order
decQty (LimitOrder i bi shi p q s mq fak) q' = limitOrder i bi shi p (q - q') s mq fak

decQty (IcebergOrder i bi shi p q s mq fak dq vq) q' = icebergOrder i bi shi p (q - q') s mq fak dq (vq - q')


removeOrderFromQueue :: Order -> OrderQueue -> OrderQueue
removeOrderFromQueue = List.deleteBy (\ o1 o2 -> oid o1 == oid o2)


replaceOrderInQueue :: OrderID -> Order -> OrderQueue -> OrderQueue
replaceOrderInQueue ooid o (h:t) = (h':t)
  where h' = if oid h == ooid then o else h

replaceOrderInQueue ooid o [] = []


findOrderFromQueueByID :: OrderID -> OrderQueue -> Maybe Order
findOrderFromQueueByID oidToRemove oq = do
    case filtered of
        (h:_)     -> Just h
        otherwise -> Nothing
  where
    filtered = List.filter (\o -> oid o == oidToRemove) oq


removeOrderFromOrderBook :: Order -> OrderBook -> OrderBook
removeOrderFromOrderBook o (OrderBook bq sq)
    | side o == Buy = OrderBook (removeOrderFromQueue o bq) sq
    | side o == Sell = OrderBook bq (removeOrderFromQueue o sq)


replaceOrderInOrderBook :: OrderID -> Order -> OrderBook -> OrderBook
replaceOrderInOrderBook ooid o (OrderBook bq sq)
    | side o == Buy = OrderBook (replaceOrderInQueue ooid o bq) sq
    | side o == Sell = OrderBook bq (replaceOrderInQueue ooid o sq)


findOrderFromOrderBookByID :: OrderID -> Side -> OrderBook ->  Maybe Order
findOrderFromOrderBookByID oid side (OrderBook bq sq)
    | side == Buy = findOrderFromQueueByID oid bq
    | side == Sell = findOrderFromQueueByID oid sq


queuesBefore :: Order -> Order -> Bool
queuesBefore o o'
    | side o == Sell && side o' == Sell = price o < price o'
    | side o == Buy && side o' == Buy = price o > price o'
    | otherwise = error "incomparable orders"


enqueueOrder :: Order -> OrderQueue -> OrderQueue
enqueueOrder (IcebergOrder i bi shi p q s mq fak dq vq) =
    enqueueOrder' (IcebergOrder i bi shi p q s mq fak dq (min q dq))

enqueueOrder (LimitOrder i bi shi p q s mq fak) =
    enqueueOrder' (LimitOrder i bi shi p q s mq fak)


enqueueOrder' :: Order -> OrderQueue -> OrderQueue
enqueueOrder' o [] = [o]

enqueueOrder' o (o1:os)
    | queuesBefore o o1 = o:(o1:os)
    | otherwise = o1:(enqueueOrder' o os)


enqueue :: Order -> OrderBook -> OrderBook
enqueue o ob
    | side o == Buy = OrderBook (enqueueOrder o $ buyQueue ob) (sellQueue ob)
    | side o == Sell = OrderBook (buyQueue ob) (enqueueOrder o $ sellQueue ob)


enqueueIcebergRemainder :: OrderQueue -> Order -> Coverage OrderQueue
enqueueIcebergRemainder os (IcebergOrder _ _ _ _ 0 _ _ _ _ _) = os `covers` "EIR-1"

enqueueIcebergRemainder os (IcebergOrder i bi shi p q s mq fak dq 0)
    | q <= dq = enqueueOrder (icebergOrder i bi shi p q s mq fak dq q) os `covers` "EIR-2"
    | otherwise = enqueueOrder (icebergOrder i bi shi p q s mq fak dq dq) os `covers` "EIR-3"


matchBuy :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchBuy o [] = (Just o, [], []) `covers` "MB-0"

matchBuy o oq@((LimitOrder i1 bi1 shi1 p1 q1 s1 mq1 fak):os)
    | p < p1 = (Just o, oq, []) `covers` "MBL-1"
    | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBL-2"
    | q == q1 = (Nothing, os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBL-3"
    | q > q1 = do
        (o', ob', ts') <- matchBuy (decQty o q1) os
        (o', ob', (Trade p1 q1 i i1 shi bi shi1 bi1):ts') `covers` "MBL-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchBuy o ((IcebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 vq1):os)
    | p < p1 = (Just o, (icebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 vq1):os, []) `covers` "MBI-1"
    | q < dq1 = (Nothing, (icebergOrder i1 bi1 shi1 p1 (q1-q) s1 mq1 fak1 dq1 (vq1-q)):os, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBI-2"
    | q == dq1 = do
        newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 dq1 0)
        (Nothing, newQueue, [Trade p1 q i i1 shi bi shi1 bi1]) `covers` "MBI-3"
    | q > dq1 = do
        newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 dq1 0)
        (o', ob', ts') <- matchBuy (decQty o dq1) newQueue
        (o', ob', (Trade p1 dq1 i i1 shi bi shi1 bi1):ts') `covers` "MBI-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o


matchSell :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchSell o [] = (Just o, [], []) `covers` "MS-0"

matchSell o oq@((LimitOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1):os)
    | p > p1 = (Just o, oq, []) `covers` "MSL-1"
    | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSL-2"
    | q == q1 = (Nothing, os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSL-3"
    | q > q1 = do
        (o', ob', ts') <- matchSell (decQty o q1) os
        (o', ob', (Trade p1 q1 i1 i shi1 bi1 shi bi):ts') `covers` "MSL-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o

matchSell o ((IcebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 vq1):os)
    | p > p1 = (Just o, (icebergOrder i1 bi1 shi1 p1 q1 s1 mq1 fak1 dq1 vq1):os, []) `covers` "MSI-1"
    | q < dq1 = (Nothing, (icebergOrder i1 bi1 shi1 p1 (q1-q) s1 mq1 fak1 dq1 (vq1-q)):os, [Trade p1 q i1 i shi1 bi1 shi bi]) `covers` "MSI-2"
    | q == dq1 = do
        newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 dq1 0)
        (Nothing, newQueue, [Trade p1 q i1 i shi1 bi1 shi bi])  `covers` "MSI-3"
    | q > dq1 = do
        newQueue <- enqueueIcebergRemainder os (icebergOrder i1 bi1 shi1 p1 (q1-dq1) s1 mq1 fak1 dq1 0)
        (o', ob', ts') <- matchSell (decQty o dq1) newQueue
        (o', ob', (Trade p1 dq1 i1 i shi1 bi1 shi bi):ts') `covers` "MSI-4"
  where
    p = price o
    q = quantity o
    i = oid o
    shi = shid o
    bi = brid o


matchNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
matchNewOrder o ob
    | side o == Buy = do
        (rem, sq, ts) <- (matchBuy o (sellQueue ob))
        case rem of
            Nothing -> (OrderBook (buyQueue ob) sq, ts) `covers` "MNO-1"
            Just o' -> (enqueue o' $ OrderBook (buyQueue ob) sq, ts) `covers` "MNO-2"
    | side o == Sell = do
        (rem, bq, ts) <- (matchSell o (buyQueue ob))
        case rem of
            Nothing -> (OrderBook bq (sellQueue ob), ts) `covers` "MNO-3"
            Just o' -> (enqueue o' $ OrderBook bq (sellQueue ob), ts) `covers` "MNO-4"


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

adjustPeakSizeOnReplace oldOrder@(IcebergOrder _ _ _ _ _ _ _ _ olddq oldps) notAdjustedNewOrder@(IcebergOrder _ _ _ _ _ _ _ _ newdq newps)
    | oldps == olddq = notAdjustedNewOrder{visibleQty = newdq}
    | oldps < olddq && oldps > newdq = notAdjustedNewOrder{visibleQty = newdq}
    | otherwise = notAdjustedNewOrder
