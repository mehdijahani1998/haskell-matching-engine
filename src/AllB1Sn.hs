module Main where
import Domain.ME
import Domain.MEService
import Test.QuickCheck


instance Arbitrary Side where
    arbitrary = elements [Buy, Sell]

instance Arbitrary Order where

    arbitrary = do
        Positive oid <- arbitrary
        Positive brid <- arbitrary
        Positive shid <- arbitrary
        Positive price <- arbitrary
        Positive quantity <- arbitrary
        side <- arbitrary
        minQty <- arbitrary
        LimitOrder oid brid shid price quantity side minQty <$> arbitrary


-- needs to be completed
-- instance Arbitrary OrderBook where 

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



ordered xs = and (zipWith (<=) xs (drop 1 xs))
insert x xs = takeWhile (<x) xs++[x]++dropWhile (<x) xs

prop_Insert x xs = ordered xs ==> ordered (insert x xs)
  where types = x::Int

prop_valueTraded trade = priceTraded trade <= valueTraded trade

-- prop_checkTradePrice 

main :: IO()
main = do
    putStrLn "Hello, what's your name?"