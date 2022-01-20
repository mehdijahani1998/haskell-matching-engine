module Infra.Decorator
    ( Handler
    , Decorator
    , PartialDecorator
    , decorateOnAccept
    ) where

import           Domain.ME
import           Infra.Coverage

type Handler = Request -> MEState -> Coverage Response
type Decorator = Handler -> Handler
type PartialDecorator = Request -> MEState -> Response -> Coverage Response


decorateOnAccept :: String -> PartialDecorator -> Decorator
decorateOnAccept stmtPrefix decorateByType handler rq s = do
    rs <- handler rq s
    case status rs of
        Accepted   -> decorateByType rq s rs
        Eliminated -> rs `covers`  (stmtPrefix ++ "-AE")
        Rejected   -> rs `covers`  (stmtPrefix ++ "-AR")
