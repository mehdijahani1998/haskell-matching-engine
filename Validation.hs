module Validation (validateOrder) where

import           Coverage
import           Decorator
import           ME


validateOrder :: Decorator
validateOrder hdlr =
    decorateOnAccept "VAL-PR" validatePriceDecorator $
    decorateOnAccept "VAL-QTY" validateQtyDecorator $
    hdlr


validatePriceDecorator :: PartialDecorator
validatePriceDecorator rq@NewOrderRq{} s rs =
    validatePrice rq s rs

validatePriceDecorator rq@ReplaceOrderRq{} s rs =
    validatePrice rq s rs

validatePriceDecorator _ _ rs =
    rs `covers` "VAL-PR-P"


validatePrice :: PartialDecorator
validatePrice rq s rs =
    if p `rem` 1 == 0
        then rs `covers` "VAL-PR-1"
        else reject rq s `covers` "VAL-PR-2"
  where
    o = order rq
    p = price o


validateQtyDecorator :: PartialDecorator
validateQtyDecorator rq@NewOrderRq{} s rs =
    validateQty rq s rs

validateQtyDecorator rq@ReplaceOrderRq{} s rs =
    validateQty rq s rs

validateQtyDecorator _ _ rs =
    rs `covers` "VAL-PR-P"


validateQty :: PartialDecorator
validateQty rq s rs =
    if q `rem` 1 == 0
        then rs `covers` "VAL-QTY-1"
        else reject rq s `covers` "VAL-QTY-2"
  where
    o = order rq
    q = quantity o
