module MEService where

import Data.Map
import Data.Maybe
import ME
import Ownership
import MinQuantity
import FillAndKill
import CreditLimit

ownershipUpperLimit :: Int
ownershipUpperLimit = 20

newOrderMatcher :: Handler
newOrderMatcher (NewOrderRq o) s = do
  (ob, ts) <- matchNewOrder o (orderBook s)
  return (NewOrderRs Accepted ts, s { orderBook = ob})


orderCanceller :: Handler
orderCanceller (CancelOrderRq _ oid side) s = do
  (ob, o) <- cancelOrder oid side (orderBook s)
  let status = if isNothing o then Rejected else Accepted
  return (CancelOrderRs status o, s { orderBook = ob})

orderReplacer :: Handler
orderReplacer (ReplaceOrderRq oldoid o) s = do
  (ob, oldo) <- cancelOrder oldoid (side o) (orderBook s)
  if isNothing oldo then
    return (ReplaceOrderRs Rejected Nothing [], s)
    else do
      (ob', ts) <- matchNewOrder o ob
      return (ReplaceOrderRs Accepted oldo ts, s { orderBook = ob'})

newOrderHandler :: Handler
newOrderHandler = 
  fillAndKillProc $ 
  minQuantityCheck $ 
  creditLimitProc $ 
  ownershipCheck ownershipUpperLimit $ 
  newOrderMatcher

cancelOrderHandler :: Handler
cancelOrderHandler =
  creditLimitProc $ 
  ownershipCheck ownershipUpperLimit $ 
  orderCanceller

replaceOrderHandler :: Handler
replaceOrderHandler = 
  creditLimitProc $ 
  ownershipCheck ownershipUpperLimit $ 
  orderReplacer

requestHandler :: Handler
requestHandler rq@(NewOrderRq o) s =
  newOrderHandler rq s

requestHandler rq@(CancelOrderRq rqid oid side) s =
  cancelOrderHandler rq s

requestHandler rq@(ReplaceOrderRq oldoid o) s =
  replaceOrderHandler rq s

requestHandler (SetCreditRq b c) s = do
  return (SetCreditRs True, s { creditInfo = (insert b c (creditInfo s)) })

requestHandler (SetOwnershipRq sh i) s = do
  return (SetOwnershipRs True, s { ownershipInfo = (insert sh i (ownershipInfo s)) })


