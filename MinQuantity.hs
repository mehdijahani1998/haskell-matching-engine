module MinQuantity (minQuantityCheck) where

import           Coverage
import           ME


minQuantityCheck :: Decorator
minQuantityCheck handler rq@(NewOrderRq o) s =
    runOnAccept handler rq s "MQC" $ \ s rs s' -> case minQty o of
        Nothing -> (rs, s') `covers` "MQC1"
        Just mq -> if sum (Prelude.map quantityTraded $ trades rs) >= mq
            then (rs, s') `covers` "MQC2"
            else (NewOrderRs Rejected [], s) `covers` "MQC3"

minQuantityCheck handler rq s = handler rq s
