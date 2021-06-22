module Decorator
    ( Handler
    , Decorator
    , PartialDecorator
    , decorateOnAccept
    ) where

import           Coverage
import           ME

type Handler = Request -> MEState -> Coverage (Response, MEState)
type Decorator = Handler -> Handler
type PartialDecorator = Request -> MEState -> Response -> MEState -> Coverage (Response, MEState)


decorateOnAccept :: String -> PartialDecorator -> Decorator
decorateOnAccept stmt decorateByType handler rq s = do
    (rs, s') <- handler rq s
    case status rs of
        Accepted -> decorateByType rq s rs s'
        Rejected -> (rs, s') `covers`  (stmt ++ "-AR")
