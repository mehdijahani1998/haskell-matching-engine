module PriceBand where

import Coverage
import ME

pricebandCheck :: Int -> Int -> Decorator
pricebandCheck minPriceBand maxPriceBand handler rq s = case rq of
  (NewOrderRq o) -> do  
    (rs, s') <- handler rq s
    case status rs of
      Accepted ->  if pricebandPreCheck minPriceBand maxPriceBand o then do
          (rs, s') `covers` "PBC1"
        else
          (NewOrderRs Rejected [], s) `covers` "PBC2"
      Rejected -> (rs, s') `covers` "PBC3"
  (CancelOrderRq rqid oid side) -> do
    (rs, s') <- handler rq s
    (rs, s') `covers` "PBC4"
  (ReplaceOrderRq oldoid o) -> do
    (rs, s') <- handler rq s
    case status rs of
      Accepted ->  if pricebandPreCheck minPriceBand maxPriceBand o then do
          (rs, s') `covers` "PBC5"
        else
          (ReplaceOrderRs Rejected Nothing [], s) `covers` "PBC6"
      Rejected -> (rs, s') `covers` "PBC7"
  _ -> handler rq s


pricebandPreCheck :: Int -> Int -> Order -> Bool
pricebandPreCheck minPriceBand maxPriceBand o =
  minPriceBand <= p && p <= maxPriceBand
  where
    p = price o
