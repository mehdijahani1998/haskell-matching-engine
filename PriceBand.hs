module PriceBand where

import Coverage
import ME

pricebandCheck :: Float -> Float -> Decorator
pricebandCheck minPriceBand maxPriceBand handler rq s = case rq of
  (NewOrderRq o) -> do  
    (rs, s') <- handler rq s
    let rp = referencePrice s
    case status rs of
      Accepted ->  if pricebandPreCheck minPriceBand maxPriceBand rp o then do
          (rs, s') `covers` "PBC1"
        else
          (NewOrderRs Rejected [], s) `covers` "PBC2"
      Rejected -> (rs, s') `covers` "PBC3"
  (CancelOrderRq rqid oid side) -> do
    (rs, s') <- handler rq s
    (rs, s') `covers` "PBC4"
  (ReplaceOrderRq oldoid o) -> do
    (rs, s') <- handler rq s
    let rp = referencePrice s
    case status rs of
      Accepted ->  if pricebandPreCheck minPriceBand maxPriceBand rp o then do
          (rs, s') `covers` "PBC5"
        else
          (ReplaceOrderRs Rejected Nothing [], s) `covers` "PBC6"
      Rejected -> (rs, s') `covers` "PBC7"
  _ -> handler rq s


pricebandPreCheck :: Float -> Float -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand referencePrice o =
  lowerPriceLimit <= p && p <= upperpriceLimit
  where
    p = price o
    upperpriceLimit = floor $ fromIntegral referencePrice * (1 + maxPriceBand)
    lowerPriceLimit = ceiling $ fromIntegral referencePrice * (1 - minPriceBand)
