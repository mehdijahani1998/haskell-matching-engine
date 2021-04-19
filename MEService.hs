module MEService where

import Data.Map
import Coverage
import ME
import Ownership
import MinQuantity
import FillAndKill
import CreditLimit

newOrderMatcher :: Handler
newOrderMatcher (NewOrderRq o) s = do
  (ob, ts) <- matchNewOrder o (orderBook s)
  return (NewOrderRs Accepted ts, s { orderBook = ob})


orderCanceller :: Handler
orderCanceller (CancelOrderRq rqid oid side) s = do
  ob <- cancelOrder oid side (orderBook s)
  return (CancelOrderRs Accepted, s { orderBook = ob})

newOrderHandler :: Handler
newOrderHandler = 
  creditLimitProc $ 
  fillAndKillProc $ 
  minQuantityCheck $ 
  (ownershipCheck 20) $ 
  newOrderMatcher

cancelOrderHandler :: Handler
cancelOrderHandler =
  orderCanceller

requestHandler :: Handler
requestHandler rq@(NewOrderRq o) s =
  newOrderHandler rq s

requestHandler rq@(CancelOrderRq rqid oid side) s =
  cancelOrderHandler rq s

requestHandler (SetCreditRq b c) s = do
  return (SetCreditRs True, s { creditInfo = (insert b c (creditInfo s)) })

requestHandler (SetOwnershipRq sh i) s = do
  return (SetOwnershipRs True, s { ownershipInfo = (insert sh i (ownershipInfo s)) })


