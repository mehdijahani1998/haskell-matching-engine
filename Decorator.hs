module Decorator
    ( Handler
    , Decorator
    , PartialDecorator
    , decorateOnAccept
    ) where

import           Coverage
import           ME

type Handler = Request -> MEState -> Coverage Response
type Decorator = Handler -> Handler
type PartialDecorator = Request -> MEState -> Response -> Coverage Response


decorateOnAccept :: String -> PartialDecorator -> Decorator
decorateOnAccept stmt decorateByType handler rq s = do
    rs <- handler rq s
    case status rs of
        Accepted   -> decorateByType rq s rs
        Eliminated -> decorateByType rq s rs
        Rejected   -> rs `covers`  (stmt ++ "-AR")
