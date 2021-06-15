module PriceBand (pricebandCheck) where

import           Coverage
import           ME


priceBandCheckOnAccept :: Float -> Float -> Order -> Response -> MEState -> Response -> MEState -> Coverage (Response, MEState)
priceBandCheckOnAccept minPriceBand maxPriceBand o emptyResponse s rs s' = do
    let rp = referencePrice s
    if pricebandPreCheck minPriceBand maxPriceBand rp o
        then (rs, s') `covers` "PBC1"
        else (emptyResponse, s) `covers` "PBC2"

pricebandCheck :: Float -> Float -> Decorator
pricebandCheck minPriceBand maxPriceBand handler rq@(NewOrderRq o) s =
    runOnAccept handler rq s "PBC" $ priceBandCheckOnAccept minPriceBand maxPriceBand o $ NewOrderRs Rejected []

pricebandCheck minPriceBand maxPriceBand handler rq@(ReplaceOrderRq _ o) s =
    runOnAccept handler rq s "PBC" $ priceBandCheckOnAccept minPriceBand maxPriceBand o $ ReplaceOrderRs Rejected Nothing []

pricebandCheck _ _ handler rq s = handler rq s


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand referencePrice o =
    lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = floor $ fromIntegral referencePrice * (1 + maxPriceBand)
    lowerPriceLimit = ceiling $ fromIntegral referencePrice * (1 - minPriceBand)
