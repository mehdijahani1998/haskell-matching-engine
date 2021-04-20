module MEService where

import Data.Map
import Data.Maybe
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
orderCanceller (CancelOrderRq _ oid side) s = do
  (ob, o) <- cancelOrder oid side (orderBook s)
  let status = if isNothing o then Rejected else Accepted
  return (CancelOrderRs status o, s { orderBook = ob})

newOrderHandler :: Handler
newOrderHandler = 
  creditLimitProc $ 
  fillAndKillProc $ 
  minQuantityCheck $ 
  (ownershipCheck 20) $ 
  newOrderMatcher

cancelOrderHandler :: Handler
cancelOrderHandler =
  creditLimitProc $ 
  (ownershipCheck 20) $ 
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


