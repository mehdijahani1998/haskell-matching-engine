module CreditLimit (creditLimitProc) where

import           Coverage
import qualified Data.Map as Map
import           ME


creditSpentByBuyer :: BrokerID -> [Trade] -> Int
creditSpentByBuyer buyerId ts =
    sum $
    map valueTraded $
    filter (\t -> sellerBrId t /= buyerId) ts


totalWorth :: Side -> ShareholderID -> OrderBook -> Int
totalWorth side shi ob =
    sum $
    map (\o -> price o * quantity o) $
    filter (\o -> shid o == shi) $
    queue side ob


creditLimitCheck :: Order -> MEState -> [Trade] -> MEState -> Bool
creditLimitCheck order beforeTradeState ts afterTradeState
    | side order == Buy  = (creditInfo beforeTradeState) Map.! buyerId >= (creditSpentByBuyer buyerId ts) + (totalWorth Buy shi afterTrade)
    | side order == Sell = True
  where
    buyerId = brid order
    afterTrade = orderBook afterTradeState
    shi = shid order


updateCreditInfo :: Order -> [Trade] -> MEState -> MEState
updateCreditInfo order ts s
    | side order == Buy  = updateBuyerCredit order ts s'
    | side order == Sell = s'
  where
    s' = updateSellersCredit ts s


updateBuyerCredit :: Order -> [Trade] -> MEState -> MEState
updateBuyerCredit buyOrder ts s@(MEState ob ci si rp) =
    MEState ob (Map.insert buyerId newCredit ci) si rp
  where
    buyerId = brid buyOrder
    newCredit = ci Map.! buyerId - (creditSpentByBuyer buyerId ts)


updateSellersCredit :: [Trade] -> MEState -> MEState
updateSellersCredit ts (MEState ob ci si rp) =
    MEState ob ci' si rp
  where
    ci' = foldl (\m t -> Map.insertWith (+) (sellerBrId t) (valueTraded t) m) ci $
        filter (\t -> buyerBrId t /= sellerBrId t) ts


creditLimitProc :: Decorator
creditLimitProc handler rq s = case rq of
    (NewOrderRq o) -> do
        (rs, s') <- handler rq s
        case status rs of
            Accepted -> if creditLimitCheck o s (trades rs) s'
                then (rs, updateCreditInfo o (trades rs) s') `covers` "CLP1"
                else (NewOrderRs Rejected [], s) `covers` "CLP2"
            Rejected -> (rs, s') `covers` "CLP3"
    (CancelOrderRq rqid oid side) -> do
        (rs, s') <- handler rq s
        (rs, s') `covers` "CLP4"
    (ReplaceOrderRq oldoid o) -> do
        (rs, s') <- handler rq s
        case status rs of
            Accepted -> do
                if creditLimitCheck o s (trades rs) s'
                    then (rs, updateCreditInfo o (trades rs) s') `covers` "CLP6"
                    else (NewOrderRs Rejected [], s) `covers` "CLP7"
            Rejected -> (rs, s') `covers` "CLP8"
    _ -> handler rq s
