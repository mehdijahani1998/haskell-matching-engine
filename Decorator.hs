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
type PartialDecorator = Request -> MEState -> Response -> MEState -> Coverage Response


decorateOnAccept :: String -> PartialDecorator -> Decorator
decorateOnAccept stmt decorateByType handler rq s = do
    rs <- handler rq s
    let s' = state rs
    case status rs of
        Accepted -> decorateByType rq s rs s'
        Rejected -> rs `covers`  (stmt ++ "-AR")
