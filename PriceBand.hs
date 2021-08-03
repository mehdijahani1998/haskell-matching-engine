module PriceBand (pricebandCheck) where

import           Coverage
import           Decorator
import           ME


pricebandCheck :: Float -> Float -> Decorator
pricebandCheck minPriceBand maxPriceBand =
    decorateOnAccept "PBC" $ pricebandCheckByType minPriceBand maxPriceBand


pricebandCheckByType :: Float -> Float -> PartialDecorator
pricebandCheckByType minPriceBand maxPriceBand rq@NewOrderRq {} s rs = do
    pricebandCheckForArrivingOrder minPriceBand maxPriceBand rq s rs

pricebandCheckByType minPriceBand maxPriceBand rq@ReplaceOrderRq {} s rs = do
    pricebandCheckForArrivingOrder minPriceBand maxPriceBand rq s rs

pricebandCheckByType _ _ _ _ rs =
    rs `covers` "PBC-P"


pricebandCheckForArrivingOrder :: Float -> Float -> PartialDecorator
pricebandCheckForArrivingOrder minPriceBand maxPriceBand rq s rs = do
    let o = order rq
    let rp = referencePrice s
    if pricebandPreCheck minPriceBand maxPriceBand rp o
        then rs `covers` "PBC1"
        else reject rq s `covers` "PBC2"


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand referencePrice o =
    lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = floor $ fromIntegral referencePrice * (1 + maxPriceBand)
    lowerPriceLimit = ceiling $ fromIntegral referencePrice * (1 - minPriceBand)
