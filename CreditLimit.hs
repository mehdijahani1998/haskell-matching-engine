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


totalWorthInQueue :: Side -> BrokerID -> OrderBook -> Int
totalWorthInQueue side bri ob =
    sum $
    map (\o -> price o * quantity o) $
    filter (\o -> brid o == bri) $
    queueBySide side ob


creditLimitCheckForArrivingOrder :: CreditManagerState state => Order -> state -> [Trade] -> state -> Bool
creditLimitCheckForArrivingOrder o beforeTradeState ts afterTradeState = do
    bri `Map.member` credits && case side o of
        Buy  -> getCreditInfo beforeTradeState Map.! bri >= creditSpentByBuyer bri ts + totalWorthInQueue Buy bri afterTrade
        Sell -> True
  where
    bri = brid o
    credits = getCreditInfo beforeTradeState
    afterTrade = getOrderBook afterTradeState


updateCreditInfo :: CreditManagerState state => [Trade] -> state -> state
updateCreditInfo ts s =
    foldl updateCreditByTrade s ts


updateCreditByTrade :: CreditManagerState state => state -> Trade -> state
updateCreditByTrade s t =
    s''
  where
    s' = updateSellerCreditByTrade s t
    s'' = updateBuyerCreditByTrade s' t


updateBuyerCreditByTrade :: CreditManagerState state => state -> Trade -> state
updateBuyerCreditByTrade state t =
    setCreditInfo state $ Map.insert bid newCredit ci
  where
    bid = buyerBrId t
    ci = getCreditInfo state
    newCredit = ci Map.! bid - valueTraded t


updateSellerCreditByTrade :: CreditManagerState state => state -> Trade -> state
updateSellerCreditByTrade state t =
    setCreditInfo state $ Map.insert sid newCredit ci
  where
    sid = sellerBrId t
    ci = getCreditInfo state
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
