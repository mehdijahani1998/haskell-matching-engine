{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where
import Domain.ME
    ( Trade(Trade, priceTraded, quantityTraded),
      OrderBook(OrderBook, buyQueue, sellQueue),
      Order(LimitOrder, quantity, fillAndKill, minQty, side, shid, price),
      Side(..),
      Quantity,
      valueTraded, OrderQueue, MEState (orderBook), OrderID, ShareholderID, BrokerID, Price,
      limitOrder )
import Domain.MEService
import Test.QuickCheck
import Decorators.OrderHandler (matchNewOrder', matchNewOrder)
import Data.Ord (comparing)
import Data.List (sortBy)


-- instance Arbitrary Side where
--     arbitrary = elements [Buy, Sell]

instance Arbitrary OrderBook where
    arbitrary = genOrderBook

instance Arbitrary Order where
    arbitrary = genRandomOrder

type MinimumQuantity = Maybe Quantity

genMinQty :: Gen MinimumQuantity
genMinQty = elements list
    where list = Nothing : [Just n | n<-[0..1000]]

genQtyandMinQty :: Gen (Quantity, MinimumQuantity)
genQtyandMinQty = elements list
    where list = [(a, Nothing) | a <- [1..1000]] ++ [(a, Just b) | a <- [1..1000], b <- [1..1000], a >= b]

genFillAndKill :: Gen Bool
genFillAndKill = elements [False]

genOnlyBuySide :: Gen Side
genOnlyBuySide = elements [Buy]

genOnlySellSide :: Gen Side
genOnlySellSide = elements [Sell]

genBothSides :: Gen Side
genBothSides = elements [Buy, Sell]

genIDs :: Gen (OrderID, BrokerID, ShareholderID)
genIDs = elements list
    where list = [(oid, brid, shid) | oid <- [1..10], brid <- [1..10], shid <- [1..10]]

genPrice :: Gen Price
genPrice = elements list
    where list = [a | a <- [1..1000]]

genOnlyBuyOrder :: Gen Order
genOnlyBuyOrder = do --Positive oid <- arbitrary
                     --Positive brid <- arbitrary
                     --Positive shid <- arbitrary
                     (oid, brid, shid) <- genIDs
                     price <- genPrice
                     buySide <- genOnlyBuySide
                     (quantity , minQty) <- genQtyandMinQty
                     fillAndKill <- genFillAndKill
                     return (LimitOrder oid brid shid price quantity buySide minQty fillAndKill)

genOnlySellOrder :: Gen Order
genOnlySellOrder = do --Positive oid <- arbitrary
                      --Positive brid <- arbitrary
                      --Positive shid <- arbitrary
                      (oid, brid, shid) <- genIDs
                      price <- genPrice
                      sellSide <- genOnlySellSide
                      (quantity , minQty) <- genQtyandMinQty
                      fillAndKill <- genFillAndKill
                      return (LimitOrder oid brid shid price quantity sellSide minQty fillAndKill)

genRandomOrder :: Gen Order
genRandomOrder = do --Positive oid <- arbitrary
                    --Positive brid <- arbitrary
                    --Positive shid <- arbitrary
                    (oid, brid, shid) <- genIDs
                    price <- genPrice
                    side <- genBothSides
                    (quantity , minQty) <- genQtyandMinQty
                    fillAndKill <- genFillAndKill
                    return (LimitOrder oid brid shid price quantity side minQty fillAndKill)

genBuyQueue :: Gen OrderQueue
genBuyQueue = listOf genOnlyBuyOrder

genSellQueue :: Gen OrderQueue
genSellQueue = listOf genOnlySellOrder

genOrderBook :: Gen OrderBook
genOrderBook = do buyQ <- genBuyQueue
                  sellQ <- genSellQueue
                  --let buyQ2 = sortBuyQueue buyQ
                  return (OrderBook buyQ sellQ)

getOrderPrice :: Order -> Price
getOrderPrice ord = price ord

sortBuyQueue = sortBy (comparing getOrderPrice)

instance Arbitrary Trade where

    arbitrary = do
        Positive priceTraded <- arbitrary
        Positive quantityTraded <- arbitrary
        Positive buyId <- arbitrary
        Positive sellId <- arbitrary
        Positive buyerShId <- arbitrary
        Positive buyerBrId <- arbitrary
        Positive sellerShId <- arbitrary
        Positive sellerBrId <- arbitrary
        return $ Trade priceTraded quantityTraded buyId sellId buyerShId buyerBrId sellerShId sellerBrId

-- Conservation of quantity property --

-- 1. Auxiliary functions
getTradesQuantitySum :: [Trade] -> Int
getTradesQuantitySum [] = 0
getTradesQuantitySum [t] = quantityTraded t
getTradesQuantitySum (t:ts) = quantityTraded t + getTradesQuantitySum ts

getOrderQueueQuantitySum :: OrderQueue -> Int
getOrderQueueQuantitySum [] = 0
getOrderQueueQuantitySum [order] = quantity order
getOrderQueueQuantitySum (ord:ob) = quantity ord + getOrderQueueQuantitySum ob

getOrderBookQuantitySum :: OrderBook -> Int
getOrderBookQuantitySum ob = getOrderQueueQuantitySum (buyQueue ob) + getOrderQueueQuantitySum (sellQueue ob)

orderBookNotNull :: OrderBook -> Bool
orderBookNotNull ob = not (null (sellQueue ob) || null (buyQueue ob))

onlyOneTrade :: [Trade] -> Bool
onlyOneTrade trd = length trd <= 1

-- 2. Property check functions
quantitySumEquityCheck :: Order -> OrderBook -> Bool
quantitySumEquityCheck newOrder orderBook = quantity newOrder + getOrderBookQuantitySum orderBook == getOrderBookQuantitySum remainOrderBook + 2 * getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

prop_quantitySumEqual :: Order -> OrderBook -> Property
prop_quantitySumEqual newOrder orderBook = orderBookNotNull orderBook ==> quantitySumEquityCheck newOrder orderBook

prop_quantitySumEqual_Classified:: Order -> OrderBook -> Property
prop_quantitySumEqual_Classified newOrder orderBook = orderBookNotNull orderBook ==> collect (length trades) $ quantitySumEquityCheck newOrder orderBook
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook


-- miscellaneous properties tests related to quantity
prop_tradesQuantitySum_check :: Order -> OrderBook -> Bool
prop_tradesQuantitySum_check newOrder orderBook =
    quantity newOrder >= getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

prop_remainQuantitySumCompare_check :: Order -> OrderBook -> Bool
prop_remainQuantitySumCompare_check newOrder orderBook =
    quantity newOrder + getOrderBookQuantitySum orderBook >= getOrderBookQuantitySum remainOrderBook
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook


-- Heads of sell queue and buy queue can be matched or not property --

-- 1. Auxiliary functions
ordersCantBeMatched :: Order -> Order -> Bool
ordersCantBeMatched bord sord
    | side bord == side sord = True
    | price bord < price sord = True
    | otherwise = False

canHeadsMatchAfter :: Order -> OrderBook -> Bool
canHeadsMatchAfter newOrder orderBook = orderBookNotNull remainOrderBook
                                        && ordersCantBeMatched buyHead sellHead
                                        || null (buyQueue remainOrderBook)
                                        || null (sellQueue remainOrderBook)
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook
          buyHead = head $ buyQueue remainOrderBook
          sellHead = head $ sellQueue remainOrderBook

canHeadsMatchBefore :: OrderBook -> Bool
canHeadsMatchBefore orderBook = ordersCantBeMatched buyHead sellHead
    where buyHead = head $ buyQueue orderBook
          sellHead = head $ sellQueue orderBook

-- 2. Property check function
prop_canHeadsMatch :: Order -> OrderBook -> Property
prop_canHeadsMatch newOrder orderBook = orderBookNotNull orderBook ==> canHeadsMatchAfter newOrder orderBook

-- Compare trades price with sell and buy queue --

tradePriceMoreThanBuyLessThanSell :: Order -> OrderBook -> Bool
tradePriceMoreThanBuyLessThanSell newOrder orderBook = if side newOrder == Buy then headTradePrice <= sellHeadPrice else headTradePrice >= buyHeadPrice
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook
          headTradePrice = if null trades then 0 else priceTraded (last trades)
          buyHeadPrice = if null $ buyQueue remainOrderBook then 0 else price (head $ buyQueue remainOrderBook)
          sellHeadPrice = if null $ sellQueue remainOrderBook then 0 else price (head $ sellQueue remainOrderBook)

prop_tradePriceCompareWithHeads_mbls :: Order -> OrderBook -> Property
prop_tradePriceCompareWithHeads_mbls newOrder orderBook = orderBookNotNull orderBook &&
                                                      orderBookNotNull remainOrderBook &&
                                                      not (null trades)
                                                      ==> tradePriceMoreThanBuyLessThanSell newOrder orderBook
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

main :: IO()
main = do
    putStrLn "Hello, what's your name?"

