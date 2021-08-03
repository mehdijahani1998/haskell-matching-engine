module Validation (validateOrder) where

import           Coverage
import           Decorator
import           ME


validateOrder :: Decorator
validateOrder hdlr =
    hdlr
