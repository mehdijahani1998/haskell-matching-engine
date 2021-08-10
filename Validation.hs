module Validation (validateOrder) where

import           Coverage
import           Decorator
import           ME


validateOrder :: Decorator
validateOrder hdlr =
    decorateOnAccept "VAL-PR" validatePriceDecorator $
    decorateOnAccept "VAL-QTY" validateQtyDecorator $
    decorateOnAccept "VAL-AttrGen" validateAttrGeneralDecorator $
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
    if p > 0 && p `rem` tick == 0
        then rs `covers` "VAL-PR-1"
        else reject rq s `covers` "VAL-PR-2"
  where
    o = order rq
    p = price o
    tick = tickSize s


validateQtyDecorator :: PartialDecorator
validateQtyDecorator rq@NewOrderRq{} s rs =
    validateQty rq s rs

validateQtyDecorator rq@ReplaceOrderRq{} s rs =
    validateQty rq s rs

validateQtyDecorator _ _ rs =
    rs `covers` "VAL-QTY-P"


validateQty :: PartialDecorator
validateQty rq s rs =
    if q > 0 && q `rem` lot == 0 && validateIcebergQty o && validateMinQty o
        then rs `covers` "VAL-QTY-1"
        else reject rq s `covers` "VAL-QTY-2"
  where
    o = order rq
    q = quantity o
    lot = lotSize s


validateIcebergQty :: Order -> Bool
validateIcebergQty LimitOrder {} =
    True

validateIcebergQty order@IcebergOrder {} =
    d > 0 && d <= q
  where
    q = quantity order
    d = disclosedQty order


validateMinQty :: Order -> Bool
validateMinQty order =
    case m of
        Nothing -> True
        Just mq -> mq > 0 && mq <= q
  where
    m = minQty order
    q = quantity order


validateAttrGeneralDecorator :: PartialDecorator
validateAttrGeneralDecorator rq@NewOrderRq{} s rs =
    validateAttrGeneral rq s rs

validateAttrGeneralDecorator rq@ReplaceOrderRq{} s rs =
    validateAttrGeneral rq s rs

validateAttrGeneralDecorator _ _ rs =
    rs `covers` "VAL-AttrGen-P"


validateAttrGeneral :: PartialDecorator
validateAttrGeneral rq s rs =
    if validateFakWithIceberg o && validateFakWithMinQty o
        then rs `covers` "VAL-AttrGen-1"
        else reject rq s `covers` "VAL-AttrGen-2"
  where
    o = order rq


validateFakWithIceberg :: Order -> Bool
validateFakWithIceberg LimitOrder {} =
    True

validateFakWithIceberg order@IcebergOrder {} =
    not fak
  where
    fak = fillAndKill order


validateFakWithMinQty :: Order -> Bool
validateFakWithMinQty order =
    case m of
        Nothing -> True
        Just _ -> not fak
  where
    fak = fillAndKill order
    m = minQty order
