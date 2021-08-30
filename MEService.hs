module MEService (requestHandler) where

import           Coverage
import           CreditLimit
import           Data.Map
import           Data.Maybe
import           Decorator
import           FillAndKill
import           ME
import           MinQuantity
import           Ownership
import           PriceBand
import           Validation


newOrderMatcher :: Handler
newOrderMatcher (NewOrderRq o) s = do
    (ob, ts) <- matchNewOrder o (orderBook s)
    return (NewOrderRs Accepted ts s { orderBook = ob})


orderCanceller :: Handler
orderCanceller (CancelOrderRq _ oid side) s = do
    let ob = orderBook s
    (ob', o) <- cancelOrder oid side ob
    let status = if isNothing o then Rejected else Accepted
    return (CancelOrderRs status o s { orderBook = ob'})


orderReplacer :: Handler
orderReplacer rq@(ReplaceOrderRq oldoid oNotAdjusted) s = do
    let ob = orderBook s
    (ob', oldo) <- cancelOrder oldoid (side oNotAdjusted) ob
    if isNothing oldo
        then return $ reject rq s
        else do
            let oldOrder = fromJust oldo
            if postponedCheckOnReplace oldOrder oNotAdjusted
                then do
                    let o = adjustPeakSizeOnReplace oldOrder oNotAdjusted
                    (ob'', ts) <- if shouldReplaceInPlace oldOrder o then replaceOrderInPlace (oid oldOrder) o ob else matchNewOrder o ob'
                    return (ReplaceOrderRs Accepted oldo ts s { orderBook = ob''})
                else return $ reject rq s


handlerSeed :: Handler
handlerSeed NewOrderRq {} s = NewOrderRs Accepted [] s `covers` "NO-RCV"

handlerSeed ReplaceOrderRq {} s = ReplaceOrderRs Accepted Nothing [] s `covers` "RO-RCV"

handlerSeed CancelOrderRq {} s = CancelOrderRs Accepted Nothing s `covers` "CO-RCV"


arrivingOrderDecorator :: Decorator
arrivingOrderDecorator =
    decorateOnAccept "PRC-" arrivingOrderDecoratorOnAccept


arrivingOrderDecoratorOnAccept :: PartialDecorator
arrivingOrderDecoratorOnAccept rq@NewOrderRq{} s _ = do
    newOrderMatcher rq s

arrivingOrderDecoratorOnAccept rq@ReplaceOrderRq {} s _ = do
    orderReplacer rq s

arrivingOrderDecoratorOnAccept rq@CancelOrderRq {} s _ = do
    orderCanceller rq s


newOrderHandler :: Handler
newOrderHandler =
    fillAndKillProc $
    minQuantityCheck $
    creditLimitProc $
    ownershipCheck $
    pricebandCheck $
    arrivingOrderDecorator $
    validateOrder
    handlerSeed


cancelOrderHandler :: Handler
cancelOrderHandler =
    creditLimitProc $
    ownershipCheck $
    pricebandCheck $
    arrivingOrderDecorator $
    validateOrder
    handlerSeed


replaceOrderHandler :: Handler
replaceOrderHandler =
    creditLimitProc $
    ownershipCheck $
    pricebandCheck $
    arrivingOrderDecorator $
    validateOrder
    handlerSeed


requestHandler :: Handler
requestHandler rq@NewOrderRq {} s =
    newOrderHandler rq s

requestHandler rq@CancelOrderRq {} s =
    cancelOrderHandler rq s

requestHandler rq@ReplaceOrderRq {} s =
    replaceOrderHandler rq s

requestHandler (SetCreditRq b c) s = do
    return (SetCreditRs Accepted s { creditInfo = insert b c (creditInfo s) })

requestHandler (SetOwnershipRq sh i) s = do
    return (SetOwnershipRs Accepted s { ownershipInfo = insert sh i (ownershipInfo s) })

requestHandler (SetReferencePriceRq rp) s = do
    return (SetReferencePriceRs Accepted s { referencePrice = rp })

requestHandler (SetTotalSharesRq ts) s = do
    return (SetTotalSharesRs Accepted s { totalShares = ts })

requestHandler (SetStaticPriceBandLowerLimitRq pb) s = do
    return (SetStaticPriceBandLowerLimitRs Accepted s { staticPriceBandLowerLimit = pb })

requestHandler (SetStaticPriceBandUpperLimitRq pb) s = do
    return (SetStaticPriceBandUpperLimitRs Accepted s { staticPriceBandUpperLimit = pb })

requestHandler (SetOwnershipUpperLimitRq ol) s = do
    return (SetOwnershipUpperLimitRs Accepted s { ownershipUpperLimit = ol })

requestHandler (SetTickSizeRq t) s = do
    return (SetTickSizeRs Accepted s { tickSize = t })

requestHandler (SetLotSizeRq l) s = do
    return (SetLotSizeRs Accepted s { lotSize = l })
