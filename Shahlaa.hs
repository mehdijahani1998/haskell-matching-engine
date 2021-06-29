module Shahlaa where

import           Control.Monad.Trans.State
import           Coverage
import qualified Data.Set                  as Set
import           ME
import           MEService
import           Text.Printf


data TestCase = TestCase
    { input    :: [Request]
    , output   :: [Response]
    , coverage :: [CoverageInfo]
    } deriving (Eq, Show)


type TestState = (MEState, [Response], [CoverageInfo])


initTestState :: TestState
initTestState = (initMEState, [], [])


handleRequest :: TestState -> Request -> TestState
handleRequest (s, rss, covs) rq =
    (s', rss ++ [rs], covs ++ [cov])
  where
    ((rs, s'), cov) = runState (requestHandler rq s) emptyCoverage


addOracle :: [Request] -> TestCase
addOracle rqs =
    TestCase rqs rss covs
  where
    (s, rss, covs) = foldl handleRequest initTestState rqs


fSide :: Side -> String
fSide Sell = "SELL"
fSide Buy  = "BUY "


fFAK :: Bool -> String
fFAK b = if b then "FAK" else "---"


fOptional :: Maybe Int -> Int
fOptional Nothing  = 0
fOptional (Just n) = n


fOrder :: Order -> String
fOrder (LimitOrder i bi shi p q s mq fak) =
    printf "Limit\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t0" i bi shi p q (fSide s) (fOptional mq) (fFAK fak)
fOrder (IcebergOrder i bi shi p q s mq fak dq ps) =
    printf "Iceberg\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t%d" i bi shi p q (fSide s) (fOptional mq) (fFAK fak) ps


fTrade :: Trade -> String
fTrade (Trade p q bid sid _ _ _ _) =
    printf "\t%d\t%d\t%d\t%d\n" p q bid sid


fTrades :: [Trade] -> String
fTrades ts = foldl (++) (printf "\tTrades\t%d\n" $ length ts) $ map fTrade ts

fRequest :: Request -> String
fRequest (NewOrderRq o) =
    printf "NewOrderRq\t\t%s\n" $ fOrder o
fRequest (CancelOrderRq rqid oid side) =
    printf "CancelOrderRq\t%d\t\t%d\t\t\t\t\t%s\n" oid rqid $ fSide side
fRequest (ReplaceOrderRq oldoid o) =
    printf "ReplaceOrderRq\t%d\t%s\n" oldoid $ fOrder o
fRequest (SetCreditRq b c) =
    printf "SetCreditRq\t%d\t%d\n" b c
fRequest (SetOwnershipRq sh i) =
    printf "SetOwnershipRq\t%d\t%d\n" sh i
fRequest (SetReferencePriceRq rp) =
    printf "SetReferencePriceRq\t%d\n" rp


fResponse :: Response -> String
fResponse (NewOrderRs s ts) =
    printf "NewOrderRs\t%s\n%s" (show s) (fTrades ts)
fResponse (CancelOrderRs s _) =
    printf "CancelOrderRs\t%s\n" (show s)
fResponse (ReplaceOrderRs s _ ts) =
    printf "ReplaceOrderRs\t%s\n%s" (show s) (fTrades ts)
fResponse (SetCreditRs s) =
    printf "SetCreditRs\t%s\n" (show s)
fResponse (SetOwnershipRs s) =
    printf "SetOwnershipRs\t%s\n" (show s)
fResponse (SetReferencePriceRs s) =
    printf "SetReferencePriceRs\t%s\n" (show s)


fInput :: [Request] -> String
fInput rqs = foldl (++) (printf "%d\n" $ length rqs) $ map fRequest rqs


fOutput :: [Response] -> String
fOutput rss = foldl (++) "" $ map fResponse rss


fTestCase :: TestCase -> String
fTestCase (TestCase inp out _) = (fInput inp) ++ (fOutput out) ++ "\n"


fTestSuite :: [TestCase] -> String
fTestSuite ts = foldl (\acc tc -> acc ++ (fTestCase tc) ++ "\n") (printf "%d\n" $ length ts) ts


coverageSetTC :: [CoverageInfo] -> Set.Set CoverageItem
coverageSetTC = Set.fromList . concat


fCoverage :: [CoverageInfo] -> String
fCoverage cs = unwords $ Set.elems $ coverageSetTC cs


fCoverageInOrder :: [CoverageInfo] -> String
fCoverageInOrder cs = unwords $ concat cs


coverageScoreTC :: TestCase -> Int
coverageScoreTC = coverageScore . concat . coverage


avgCoverageScoreTS :: [TestCase] -> Double
avgCoverageScoreTS ts = (fromIntegral $ sum $ map coverageScoreTC ts) / (fromIntegral $ length ts)
