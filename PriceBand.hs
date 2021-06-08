module PriceBand (pricebandCheck) where

import           Coverage
import           ME


pricebandCheck :: Float -> Float -> Decorator
pricebandCheck minPriceBand maxPriceBand handler rq s = do
    (rs, s') <- handler rq s
    case status rs of
        Accepted -> case rq of
            (NewOrderRq o) -> do
                let rp = referencePrice s
                if pricebandPreCheck minPriceBand maxPriceBand rp o
                    then (rs, s') `covers` "PBC1"
                    else (NewOrderRs Rejected [], s) `covers` "PBC2"
            (ReplaceOrderRq oldoid o) -> do
                let rp = referencePrice s
                if pricebandPreCheck minPriceBand maxPriceBand rp o
                    then (rs, s') `covers` "PBC3"
                    else (ReplaceOrderRs Rejected Nothing [], s) `covers` "PBC4"
            _ -> (rs, s') `covers` "PBC5"
        Rejected -> (rs, s') `covers`  "PBC6"


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand referencePrice o =
    lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = floor $ fromIntegral referencePrice * (1 + maxPriceBand)
    lowerPriceLimit = ceiling $ fromIntegral referencePrice * (1 - minPriceBand)
