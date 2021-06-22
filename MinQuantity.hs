module MinQuantity (minQuantityCheck) where

import           Coverage
import           ME


minQuantityCheck :: Decorator
minQuantityCheck =
    decorateOnAccept "MQC" pricebandCheckByType


pricebandCheckByType :: PartialDecorator
pricebandCheckByType (NewOrderRq o) s rs s' =
    case minQty o of
        Nothing -> (rs, s') `covers` "MQC1"
        Just mq -> if sum (Prelude.map quantityTraded $ trades rs) >= mq
            then (rs, s') `covers` "MQC2"
            else (NewOrderRs Rejected [], s) `covers` "MQC3"

pricebandCheckByType _ _ rs s' =
    (rs, s') `covers`  "MQC4"
