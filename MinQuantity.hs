module MinQuantity (minQuantityCheck) where

import           Coverage
import           ME


minQuantityCheck :: Decorator
minQuantityCheck handler rq s = do
    (rs, s') <- handler rq s
    case status rs of
        Accepted -> case rq of
            (NewOrderRq o) -> case minQty o of
                Nothing -> (rs, s') `covers` "MQC1"
                Just mq -> if sum (Prelude.map quantityTraded $ trades rs) >= mq
                    then (rs, s') `covers` "MQC2"
                    else (NewOrderRs Rejected [], s) `covers` "MQC3"
            _ -> (rs, s') `covers` "MQC4"
        Rejected -> (rs, s') `covers`  "MQC5"
