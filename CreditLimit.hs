module CreditLimit (creditLimitProc) where

import           Coverage
import qualified Data.Map  as Map
import           Decorator
import           ME


creditSpentByBuyer :: BrokerID -> [Trade] -> Int
creditSpentByBuyer buyerId ts =
    sum $
    map valueTraded $
    filter (\t -> sellerBrId t /= buyerId) ts


totalWorthInQueue :: Side -> ShareholderID -> OrderBook -> Int
totalWorthInQueue side shi ob =
    sum $
    map (\o -> price o * quantity o) $
    filter (\o -> shid o == shi) $
    queueBySide side ob


creditLimitCheckForArrivingOrder :: Order -> MEState -> [Trade] -> MEState -> Bool
creditLimitCheckForArrivingOrder o beforeTradeState ts afterTradeState = do
    bri `Map.member` credits && case side o of
        Buy  -> creditInfo beforeTradeState Map.! bri >= creditSpentByBuyer bri ts + totalWorthInQueue Buy shi afterTrade
        Sell -> True
  where
    credits = creditInfo beforeTradeState
    bri = brid o
    afterTrade = orderBook afterTradeState
    shi = shid o


updateCreditInfo :: [Trade] -> MEState -> MEState
updateCreditInfo ts s =
    foldl updateCreditByTrade s ts


updateCreditByTrade :: MEState -> Trade -> MEState
updateCreditByTrade s t =
    s''
  where
    s' = updateSellerCreditByTrade s t
    s'' = updateBuyerCreditByTrade s' t


updateBuyerCreditByTrade :: MEState -> Trade -> MEState
updateBuyerCreditByTrade (MEState ob ci si rp) t =
    MEState ob (Map.insert bid newCredit ci) si rp
  where
    bid = buyerBrId t
    newCredit = ci Map.! bid - valueTraded t


updateSellerCreditByTrade :: MEState -> Trade -> MEState
updateSellerCreditByTrade (MEState ob ci si rp) t =
    MEState ob (Map.insert sid newCredit ci) si rp
  where
    sid = sellerBrId t
    newCredit = ci Map.! sid + valueTraded t


creditLimitProc :: Decorator
creditLimitProc =
    decorateOnAccept "CLP" creditLimitProcByType


creditLimitProcByType :: PartialDecorator
creditLimitProcByType rq@NewOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType rq@ReplaceOrderRq {} s rs =
    creditLimitProcForArrivingOrder rq s rs

creditLimitProcByType _ _ rs =
    rs `covers` "CLP-P"


creditLimitProcForArrivingOrder :: PartialDecorator
creditLimitProcForArrivingOrder rq s rs = do
    let o = order rq
    let s' = state rs
    if creditLimitCheckForArrivingOrder o s (trades rs) s'
        then rs { state = updateCreditInfo (trades rs) s'} `covers` "CLP1"
        else reject rq s `covers` "CLP2"
