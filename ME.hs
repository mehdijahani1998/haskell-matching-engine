module ME where
import Text.Printf
import Control.Exception
import Coverage

type Quantity = Int
type Price = Int
type TimeStamp = Int
type OrderID = Int

data Side = Buy | Sell deriving (Show, Eq, Ord)

data Order = LimitOrder 
  { oid :: OrderID
  , price :: Price
  , quantity :: Quantity
  , side :: Side
  , minQty :: Maybe Quantity
  } | IcebergOrder 
  { oid :: OrderID
  , price :: Price
  , quantity :: Quantity
  , side :: Side
  , minQty :: Maybe Quantity
  , disclosedQty :: Quantity
  , peakSize :: Quantity
  } deriving (Show, Eq)

limitOrder i p q s m =
  assert (i >= 0) $
  assert (p > 0) $
  assert (q > 0) $
  case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
  LimitOrder i p q s m

icebergOrder i p q s m dq ps =
  assert (i >= 0) $
  assert (p > 0) $
  assert (q >= 0) $
  case m of {(Just mq) -> assert (mq > 0); otherwise -> id} $
  assert (dq <= q) $
  assert (ps > 0) $
  IcebergOrder i p q s m dq ps

type OrderQueue = [Order]

data OrderBook = OrderBook {
    buyQueue :: OrderQueue
  , sellQueue :: OrderQueue
  } deriving (Show, Eq)

data Trade = Trade {
    priceTraded :: Price
  , quantityTraded :: Quantity
  , buyId :: OrderID
  , sellId :: OrderID
  } deriving (Show, Eq)

decQty :: Order -> Quantity -> Order
decQty (LimitOrder i p q s mq) q' = limitOrder i p (q - q') s mq
decQty (IcebergOrder i p q s mq dq ps) q' = icebergOrder i p (q - q') s mq (dq -q') ps

queuesBefore :: Order -> Order -> Bool
queuesBefore o o'
  | (side o == Sell) && (side o' == Sell) = (price o < price o')
  | (side o == Buy) && (side o' == Buy) = (price o > price o')
  | otherwise = error "incomparable orders"

enqueueOrder :: Order -> OrderQueue -> OrderQueue
enqueueOrder (IcebergOrder i p q s mq dq ps) =
  enqueueOrder' (IcebergOrder i p q s mq (min q ps) ps)
enqueueOrder (LimitOrder i p q s mq) =
  enqueueOrder' (LimitOrder i p q s mq)

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
enqueueIcebergRemainder os (IcebergOrder _ _ 0 _ _ _ _) = os `covers` "EIR-1"
-- enqueueIcebergRemainder os (IcebergOrder i p q s mq 0 ps) =
--   enqueueOrder (icebergOrder i p q s mq (min q ps) ps) os
enqueueIcebergRemainder os (IcebergOrder i p q s mq 0 ps)
  | q <= ps = enqueueOrder (icebergOrder i p q s mq q ps) os `covers` "EIR-2"
  | otherwise = enqueueOrder (icebergOrder i p q s mq ps ps) os `covers` "EIR-3"

matchBuy :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchBuy o [] = (Just o, [], []) `covers` "MB-0"
matchBuy o oq@((LimitOrder i1 p1 q1 s1 mq1):os)
  | p < p1 = (Just o, oq, []) `covers` "MBL-1"
  | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i i1]) `covers` "MBL-2"
  | q == q1 = (Nothing, os, [Trade p1 q i i1]) `covers` "MBL-3"
  | q > q1 = do
      (o', ob', ts') <- matchBuy (decQty o q1) os
      (o', ob', (Trade p1 q1 i i1):ts') `covers` "MBL-4"
  where
    p = price o
    q = quantity o
    i = oid o

matchBuy o ((IcebergOrder i1 p1 q1 s1 mq1 dq1 ps1):os)
  | p < p1 = (Just o, (icebergOrder i1 p1 q1 s1 mq1 dq1 ps1):os, []) `covers` "MBI-1"
  | q < dq1 = (Nothing, (icebergOrder i1 p1 (q1-q) s1 mq1 (dq1-q) ps1):os, [Trade p1 q i i1]) `covers` "MBI-2"
  | q == dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 p1 (q1-dq1) s1 mq1 0 ps1)
      (Nothing, newQueue, [Trade p1 q i i1]) `covers` "MBI-3"
  | q > dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 p1 (q1-dq1) s1 mq1 0 ps1)
      (o', ob', ts') <- matchBuy (decQty o dq1) newQueue
      (o', ob', (Trade p1 dq1 i i1):ts') `covers` "MBI-4"
  where
    p = price o
    q = quantity o
    i = oid o

matchSell :: Order -> OrderQueue -> Coverage (Maybe Order, OrderQueue, [Trade])
matchSell o [] = (Just o, [], []) `covers` "MS-0"

matchSell o oq@((LimitOrder i1 p1 q1 s1 mq1):os)
  | p > p1 = (Just o, oq, []) `covers` "MSL-1"
  | q < q1 = (Nothing, (decQty (head oq) q):os, [Trade p1 q i1 i]) `covers` "MSL-2"
  | q == q1 = (Nothing, os, [Trade p1 q i1 i]) `covers` "MSL-3"
  | q > q1 = do
      (o', ob', ts') <- matchSell (decQty o q1) os
      (o', ob', (Trade p1 q1 i1 i):ts') `covers` "MSL-4"
  where
    p = price o
    q = quantity o
    i = oid o

matchSell o ((IcebergOrder i1 p1 q1 s1 mq1 dq1 ps1):os)
  | p > p1 = (Just o, (icebergOrder i1 p1 q1 s1 mq1 dq1 ps1):os, []) `covers` "MSI-1"
  | q < dq1 = (Nothing, (icebergOrder i1 p1 (q1-q) s1 mq1 (dq1-q) ps1):os, [Trade p1 q i1 i]) `covers` "MSI-2"
  | q == dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 p1 (q1-dq1) s1 mq1 0 ps1)
      (Nothing, newQueue, [Trade p1 q i1 i])  `covers` "MSI-3"
  | q > dq1 = do
      newQueue <- enqueueIcebergRemainder os (icebergOrder i1 p1 (q1-dq1) s1 mq1 0 ps1)
      (o', ob', ts') <- matchSell (decQty o dq1) newQueue
      (o', ob', (Trade p1 dq1 i1 i):ts') `covers` "MSI-4"
  where
    p = price o
    q = quantity o
    i = oid o

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


handleNewOrder :: Order -> OrderBook -> Coverage (OrderBook, [Trade])
handleNewOrder o ob = do
  (ob', ts') <- matchNewOrder o ob
  case minQty o of
    Nothing -> (ob', ts') `covers` "HNO-1"
    Just mq -> 
      if (sum $ map quantityTraded ts') >= mq then
        (ob', ts') `covers` "HNO-2"
      else
        (ob, []) `covers` "HNO-4"
