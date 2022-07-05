{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where
import Domain.ME
    ( Trade(Trade, priceTraded, quantityTraded),
      OrderBook(OrderBook, buyQueue, sellQueue),
      Order(LimitOrder, quantity, fillAndKill, minQty),
      Side(..),
      Quantity,
      valueTraded, OrderQueue, MEState (orderBook), OrderID, ShareholderID, BrokerID, Price )
import Domain.MEService
import Test.QuickCheck
import Decorators.OrderHandler (matchNewOrder')


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
genOnlyBuyOrder = do Positive oid <- arbitrary
                     Positive brid <- arbitrary
                     Positive shid <- arbitrary
                     price <- genPrice
                     buySide <- genOnlyBuySide
                     (quantity , minQty) <- genQtyandMinQty
                     fillAndKill <- genFillAndKill
                     return (LimitOrder oid brid shid price quantity buySide minQty fillAndKill)

genOnlySellOrder :: Gen Order
genOnlySellOrder = do Positive oid <- arbitrary
                      Positive brid <- arbitrary
                      Positive shid <- arbitrary
                      price <- genPrice
                      sellSide <- genOnlySellSide
                      (quantity , minQty) <- genQtyandMinQty
                      fillAndKill <- genFillAndKill
                      return (LimitOrder oid brid shid price quantity sellSide minQty fillAndKill)

genRandomOrder :: Gen Order 
genRandomOrder = do Positive oid <- arbitrary
                    Positive brid <- arbitrary
                    Positive shid <- arbitrary
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
                  return (OrderBook buyQ sellQ) 


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

prop_valueTraded trade = priceTraded trade <= valueTraded trade

prop_quantitySum_check :: Order -> OrderBook -> Bool
prop_quantitySum_check newOrder orderBook =
    quantity newOrder >= getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

prop_quantitySumEqual_check :: Order -> OrderBook -> Bool
prop_quantitySumEqual_check newOrder orderBook =
    quantity newOrder + getOrderBookQuantitySum orderBook >= getOrderBookQuantitySum remainOrderBook
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

quantitySumEquityCheck :: Order -> OrderBook -> Bool
quantitySumEquityCheck newOrder orderBook = quantity newOrder + getOrderBookQuantitySum orderBook == getOrderBookQuantitySum remainOrderBook + 2 * getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook


orderBookNotNull :: OrderBook -> Bool
orderBookNotNull ob = not (null (sellQueue ob) || null (buyQueue ob))

prop_quantitySumEqual_checkClassified :: Order -> OrderBook -> Property
prop_quantitySumEqual_checkClassified newOrder orderBook = orderBookNotNull orderBook ==> quantitySumEquityCheck newOrder orderBook

prop_dummyQuantity_check :: Order -> Bool
prop_dummyQuantity_check newOrder = Just (quantity newOrder) >= minQty newOrder

prop_dummyQuantity_check2 :: OrderBook -> Bool
prop_dummyQuantity_check2 orderBook = not (any prop_dummyQuantity_check (buyQueue orderBook))


main :: IO()
main = do
    putStrLn "Hello, what's your name?"