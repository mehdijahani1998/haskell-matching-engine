import           Control.Monad
import           Data.List.Index
import           System.Environment
import           System.Exit
import           System.IO

import           ME
import           Shahlaa


main :: IO()
main = do
    args <- getArgs

    when (length args /= 2) ( do
        progName <- getProgName
        hPutStrLn stderr $ "Usage:\t./" ++ progName ++ " --trades|--traces <input_file>"
        exitFailure
        )
    let func = head args
    let addr = args !! 1
    when (func /= "--trades" && func /= "--traces")
        $ error $ "Wrong func " ++ func

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

    if func == "--trades"
        then putStrLn $ fTestCase tc
        -- else putStrLn $ fCoverage $ coverage tc
        else putStrLn $ fCoverageInOrder $ coverage tc

    hClose handle


genFixture :: [String] -> Request
genFixture (t:spec)
    | t == "SetCreditRq" = genSetCreditRq spec
    | t == "SetOwnershipRq" = genSetOwnershipRq spec
    | t == "SetReferencePrice" = genSetReferencePriceRq spec
    | t == "SetTotalShares" = genSetTotalSharesRq spec
    | otherwise = error $ "Invalid Fixture Request type " ++ t


genSetCreditRq :: [String] -> Request
genSetCreditRq spec =
    SetCreditRq brokerID credit
  where
        brokerID = read $ spec !! 0 :: BrokerID
        credit = read $ spec !! 1 :: Int


genSetOwnershipRq :: [String] -> Request
genSetOwnershipRq spec =
    SetOwnershipRq shareholderID credit
  where
        shareholderID = read $ spec !! 0 :: ShareholderID
        credit = read $ spec !! 1 :: Int


genSetReferencePriceRq :: [String] -> Request
genSetReferencePriceRq spec =
    SetReferencePriceRq referencePrice
  where
        referencePrice = read $ spec !! 0 :: Price


genSetTotalSharesRq :: [String] -> Request
genSetTotalSharesRq spec =
    SetTotalSharesRq totalShares
  where
        totalShares = read $ spec !! 0 :: Quantity


genOrderRq :: OrderID -> [String] -> Request
genOrderRq newoid (t:spec)
    | t == "NewOrderRq" = NewOrderRq $ genOrder newoid spec
    | t == "ReplaceOrderRq" = genReplaceOrderRq newoid spec
    | t == "CancelOrderRq" = genCancelOrderRq newoid spec
    | otherwise = error $ "Invalid Order Request type " ++ t


genOrder :: OrderID -> [String] -> Order
genOrder newoid spec =
    if isIceberge
        then icebergOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK disclosedQty disclosedQty
        else limitOrder newoid brokerId shareholderID price qty (if isBuy then Buy else Sell) (if hasMQ then Just minQty else Nothing) isFAK
  where
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


genCancelOrderRq :: OrderID -> [String] -> Request
genCancelOrderRq newoid spec =
    CancelOrderRq newoid oid side
  where
        oid = read $ spec !! 0 :: OrderID
        isBuy = read $ spec !! 1 :: Bool
        side = if isBuy then Buy else Sell


genReplaceOrderRq :: OrderID -> [String] -> Request
genReplaceOrderRq newoid spec =
    ReplaceOrderRq oldoid o
  where
        oldoid = read $ head spec :: OrderID
        o = genOrder newoid $ tail spec
