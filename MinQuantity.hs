module MinQuantity (minQuantityCheck) where

import           Coverage
import           Decorator
import           ME


minQuantityCheck :: Decorator
minQuantityCheck =
    decorateOnAccept "MQC" pricebandCheckByType


pricebandCheckByType :: PartialDecorator
pricebandCheckByType rq@(NewOrderRq o) s rs =
    case minQty o of
        Nothing -> rs `covers` "MQC1"
        Just mq -> if sum (Prelude.map quantityTraded $ trades rs) >= mq
            then rs `covers` "MQC2"
            else (reject rq s) { status = Eliminated } `covers` "MQC3"

pricebandCheckByType _ _ rs =
    rs `covers`  "MQC-P"
