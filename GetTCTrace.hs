import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Data.List.Index

import ME
import Shahlaa

main :: IO()
main = do
    args <- getArgs

    when (length args /= 1) ( do
        progName <- getProgName
        hPutStrLn stderr $ "Usage:\t./" ++ progName ++ " input_file"
        exitFailure
        )
    let addr = head args

    handle <- openFile addr ReadMode
    contents <- hGetContents handle

    let cntLines = lines contents

    let fixtureSize = read $ head cntLines :: Int
    let tcSize = read $ cntLines !! 1 :: Int
    let rawFixtures = (take fixtureSize . drop 2) cntLines
    let rawOrders = drop (2 + fixtureSize) cntLines

    when (length rawOrders /= tcSize)
        $ error "Wrong fixtureSize and tcSize"

    let fixtures = [genFixture $ words rawFixture | rawFixture <- rawFixtures]
    let orders = [genOrderRq i $ words rawOrder | (i, rawOrder) <- indexed rawOrders]

    let tc = addOracle $ fixtures ++ orders
    -- putStrLn $ fTestCase tc
    -- putStrLn $ fCoverageInOrder $ coverage tc
    putStrLn $ fCoverage $ coverage tc
    hClose handle


genFixture :: [String] -> Request
genFixture spec = if head spec == "SetCreditRq"
    then genSetCreditRq $ tail spec
    else genSetOwnershipRq $ tail spec

genSetCreditRq :: [String] -> Request
genSetCreditRq spec = let
        brokerID = read $ spec !! 0 :: BrokerID
        credit = read $ spec !! 1 :: Int
        req = SetCreditRq brokerID credit
    in req

genSetOwnershipRq :: [String] -> Request
genSetOwnershipRq spec = let
        shareholderID = read $ spec !! 0 :: ShareholderID
        credit = read $ spec !! 1 :: Int
        req = SetOwnershipRq shareholderID credit
    in req

genOrderRq :: OrderID -> [String] -> Request
genOrderRq newoid spec = NewOrderRq $ genOrder newoid spec


genOrder :: OrderID -> [String] -> Order
genOrder newoid spec = let
        brokerId = read $ spec !! 0 :: BrokerID
        shareholderID = read $ spec !! 1 :: ShareholderID
        price = read $ spec !! 2 :: Price
        qty = read $ spec !! 3 :: Quantity
        isBuy = read $ spec !! 4 :: Bool
        minQty = read $ spec !! 5 :: Quantity
        hasMQ = minQty > 0
        isFAK = read $ spec !! 6 :: Bool
        disclosedQty = read $ spec !! 7 :: Quantity
        isIceberge = disclosedQty > 0
        ord = if isIceberge 
            then icebergOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK disclosedQty disclosedQty
            else limitOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK
    in ord