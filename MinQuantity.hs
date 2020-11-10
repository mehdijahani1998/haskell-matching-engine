module MinQuantity where

import Data.Map
import Coverage
import ME

minQuantityCheck :: Decorator
minQuantityCheck handler =
  \rq s ->
    case rq of
      (NewOrderRq o) -> do
        { (rs, s') <- handler rq s
        ; case minQty o of
            Nothing -> (rs, s') `covers` "MQC1"
            Just mq ->
              if (sum $ Prelude.map quantityTraded $ trades rs) >= mq then
                (rs, s') `covers` "MQC2"
              else
                (NewOrderRs Rejected [], s) `covers` "MQC3"
        }
      _ -> handler rq s

