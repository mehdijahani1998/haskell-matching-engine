module Main where
import Domain.ME
    ( Trade(Trade, priceTraded, quantityTraded),
      OrderBook(OrderBook, buyQueue, sellQueue),
      Order(LimitOrder, quantity, fillAndKill, minQty),
      Side(..),
      Quantity,
      valueTraded, OrderQueue, MEState (orderBook) )
import Domain.MEService
import Test.QuickCheck
import Decorators.OrderHandler (matchNewOrder')


instance Arbitrary Side where
    arbitrary = elements [Buy, Sell]

instance Arbitrary OrderBook where
    arbitrary = do
        buyQueue <- arbitrary
        OrderBook buyQueue <$> arbitrary

instance Arbitrary Order where

    arbitrary = do
        Positive oid <- arbitrary
        Positive brid <- arbitrary
        Positive shid <- arbitrary
        Positive price <- arbitrary
        -- Positive quantity <- arbitrary
        side <- arbitrary
        -- minQty <- genMinQty
        (quantity , minQty) <- genQtyandMinQty
        fillAndKill <- genFillAndKill
        LimitOrder oid brid shid price quantity side minQty <$> arbitrary

type MinimumQuantity = Maybe Quantity

genMinQty :: Gen MinimumQuantity
genMinQty = elements list
    where list = Nothing : [Just n | n<-[0..1000]]

genQtyandMinQty :: Gen (Quantity, MinimumQuantity)
genQtyandMinQty = elements list
    where list = [(a, Nothing) | a <- [1..1000]] ++ [(a, Just b) | a <- [0..1000], b <- [0..1000], a > b]

genFillAndKill :: Gen Bool
genFillAndKill = elements [False]


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

-- ordered xs = and (zipWith (<=) xs (drop 1 xs))
-- insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

-- prop_Insert x xs = ordered xs ==> ordered (insert x xs)
--   where types = x::Int

prop_valueTraded trade = priceTraded trade <= valueTraded trade


prop_quantitySum_check :: Order -> OrderBook -> Bool
prop_quantitySum_check newOrder orderBook =
    quantity newOrder >= getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

prop_quantitySumEqual_check :: Order -> OrderBook -> Bool
prop_quantitySumEqual_check newOrder orderBook =
    quantity newOrder + getOrderBookQuantitySum orderBook == getOrderBookQuantitySum remainOrderBook + 2 * getTradesQuantitySum trades
    where (remainOrderBook, trades) = matchNewOrder' newOrder orderBook

main :: IO()
main = do
    putStrLn "Hello, what's your name?"