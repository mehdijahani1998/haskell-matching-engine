module CreditLimit where

import qualified Data.Map as Map
import Coverage
import ME

creditSpentByBuyer :: BrokerID -> [Trade] -> Int
creditSpentByBuyer buyerId ts = 
  sum $ 
  map valueTraded $ 
  filter (\t -> sellerBrId t /= buyerId) ts

creditBlocked :: BrokerID -> Order -> MEState -> Int
creditBlocked buyerId buyOrder afterTradeState =
  sum $ 
  map (\o -> price o * quantity o) $ 
  filter (\o -> oid o == oid buyOrder) $
  buyQueue $
  orderBook afterTradeState

creditLimitCheck :: Order -> MEState -> [Trade] -> MEState -> Bool
creditLimitCheck order beforeTradeState ts afterTradeState
  | side order == Buy =
    let
      buyerId = brid order
    in
      (creditInfo beforeTradeState) Map.! buyerId >= (creditSpentByBuyer buyerId ts) + (creditBlocked buyerId order afterTradeState)
  | side order == Sell = True
  
updateCreditInfo :: Order -> [Trade] -> MEState -> MEState
updateCreditInfo order ts s =
  let
    s' = updateSellersCredit ts s
  in
    if side order == Buy then 
      updateBuyerCredit order ts s'
    else
      s'

updateBuyerCredit :: Order -> [Trade] -> MEState -> MEState
updateBuyerCredit buyOrder ts s@(MEState ob ci si) =
  let
    buyerId = brid buyOrder
    newCredit = ci Map.! buyerId - (creditSpentByBuyer buyerId ts) + (creditBlocked buyerId buyOrder s)
  in 
    (MEState ob (Map.insert buyerId newCredit ci) si)

updateSellersCredit :: [Trade] -> MEState -> MEState
updateSellersCredit ts (MEState ob ci si) =
  let
    ci' = 
      foldl (\m t -> Map.insertWith (+) (sellerBrId t) (valueTraded t) m) ci $
      filter (\t -> buyerBrId t /= sellerBrId t) ts
  in
    (MEState ob ci' si)

creditLimitProc :: Decorator
creditLimitProc handler = 
  \rq s ->
    case rq of
      (NewOrderRq o) -> do
        { (rs, s') <- handler rq s
        ; if creditLimitCheck o s (trades rs) s' then
            (rs, updateCreditInfo o (trades rs) s') `covers` "CLP1"
          else
            (NewOrderRs Rejected [], s) `covers` "CLP2"
        }
      _ -> handler rq s

