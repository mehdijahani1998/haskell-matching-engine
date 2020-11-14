import System.IO
import System.Environment
import System.Exit
import Control.Monad

import ME
import Shahlaa

main = do
    args <- getArgs

    when (length args /= 1) ( do
        progName <- getProgName
        hPutStrLn stderr $ "Usage:\t./" ++ progName ++ " input_file"
        exitFailure
        )
    let addr = head args

    let list = []
    handle <- openFile addr ReadMode
    contents <- hGetContents handle

    let singlewords = words contents

    let fixtureSize = read $ head singlewords :: Int
    let tcSize = read $ (head . drop 1) singlewords :: Int
    let rawFixtures = (take (fixtureSize * 3) . drop (2)) singlewords
    let rawOrders = (drop (2 + fixtureSize * 3)) singlewords

    let fixtures = [genFixture ((take 3 . drop (i * 3)) rawFixtures) | i <- [0..(fixtureSize-1)]]
    let orders = [genOrderRq i ((take 8 . drop (i * 8)) rawOrders) | i <- [0..(tcSize-1)]]

    let tc = addOracle $ fixtures ++ orders
    putStrLn $ fTestCase tc
    -- putStrLn $ fCoverage $ coverage tc
    hClose handle


genFixture :: [String] -> Request
genFixture spec = if (head spec) == "SetCreditRq"
    then genSetCreditRq $ tail spec
    else genSetOwnershipRq $ tail spec

genSetCreditRq :: [String] -> Request
genSetCreditRq spec = let
        brokerID = read $ (spec !! 0) :: BrokerID
        credit = read $ (spec !! 1) :: Int
        req = SetCreditRq brokerID credit
    in req

genSetOwnershipRq :: [String] -> Request
genSetOwnershipRq spec = let
        shareholderID = read $ (spec !! 0) :: ShareholderID
        credit = read $ (spec !! 1) :: Int
        req = SetOwnershipRq shareholderID credit
    in req

genOrderRq :: OrderID -> [String] -> Request
genOrderRq newoid spec = NewOrderRq $ genOrder newoid spec


genOrder :: OrderID -> [String] -> Order
genOrder newoid spec = let
        brokerId = read $ (spec !! 0) :: BrokerID
        shareholderID = read $ (spec !! 1) :: ShareholderID
        price = read $ (spec !! 2) :: Price
        qty = read $ (spec !! 3) :: Quantity
        isBuy = read $ (spec !! 4) :: Bool
        minQty = read $ (spec !! 5) :: Quantity
        hasMQ = minQty > 0
        isFAK = read $ (spec !! 6) :: Bool
        disclosedQty = read $ (spec !! 7) :: Quantity
        isIceberge = disclosedQty > 0
        ord = if isIceberge 
            then icebergOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK disclosedQty disclosedQty
            else (limitOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing)) isFAK
    in ord