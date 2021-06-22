module PriceBand (pricebandCheck) where

import           Coverage
import           Decorator
import           ME


pricebandCheck :: Float -> Float -> Decorator
pricebandCheck minPriceBand maxPriceBand =
    decorateOnAccept "PBC" $ pricebandCheckByType minPriceBand maxPriceBand


pricebandCheckByType :: Float -> Float -> PartialDecorator
pricebandCheckByType minPriceBand maxPriceBand (NewOrderRq o) s rs s' = do
    let rp = referencePrice s
    if pricebandPreCheck minPriceBand maxPriceBand rp o
        then (rs, s') `covers` "PBC1"
        else (NewOrderRs Rejected [], s) `covers` "PBC2"

pricebandCheckByType minPriceBand maxPriceBand (ReplaceOrderRq _ o) s rs s' = do
    let rp = referencePrice s
    if pricebandPreCheck minPriceBand maxPriceBand rp o
        then (rs, s') `covers` "PBC3"
        else (ReplaceOrderRs Rejected Nothing [], s) `covers` "PBC4"

pricebandCheckByType _ _ _ _ rs s' =
    (rs, s') `covers` "PBC5"


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand referencePrice o =
    lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = floor $ fromIntegral referencePrice * (1 + maxPriceBand)
    lowerPriceLimit = ceiling $ fromIntegral referencePrice * (1 - minPriceBand)
