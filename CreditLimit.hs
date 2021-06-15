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
    queueBySide side ob


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


creditLimitProcOnAccept :: Order -> Response -> MEState -> Response -> MEState -> Coverage (Response, MEState)
creditLimitProcOnAccept o emptyResponse s rs s' =
    if creditLimitCheck o s (trades rs) s'
        then (rs, updateCreditInfo o (trades rs) s') `covers` "CLP1"
        else (emptyResponse, s) `covers` "CLP2"

creditLimitProc :: Decorator
creditLimitProc handler rq@(NewOrderRq o) s =
    runOnAccept handler rq s "CLP" $ creditLimitProcOnAccept o $ NewOrderRs Rejected []

creditLimitProc handler rq@(ReplaceOrderRq _ o) s =
    runOnAccept handler rq s "CLP" $ creditLimitProcOnAccept o $ ReplaceOrderRs Rejected Nothing []

creditLimitProc handler rq s = handler rq s
