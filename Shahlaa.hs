module Shahlaa where

import qualified Data.Set as Set -- From the 'containers' library
import Text.Printf
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Coverage
import ME
import MEService

data TestCase = TestCase {
    input :: [Request]
  , output :: [Response]
  , coverage :: [CoverageInfo]
  } deriving (Eq, Show)

type TestState = (MEState, [Response], [CoverageInfo])
initTestState :: TestState
initTestState = (initMEState, [], [])

handleRequest :: TestState -> Request -> TestState
handleRequest (s, rss, covs) rq =
  let 
    ((rs, s'), cov) = runState (requestHandler rq s) emptyCoverage
  in
    (s', rss ++ [rs], covs ++ [cov])

addOracle :: [Request] -> TestCase
addOracle rqs = 
  let 
    (s, rss, covs) = foldl handleRequest initTestState rqs
  in
    TestCase rqs rss covs

fSide :: Side -> String
fSide Sell = "SELL"
fSide Buy = "BUY "

fFAK :: Bool -> String
fFAK b = if b then "FAK" else "---"

fOptional :: Maybe Int -> Int
fOptional Nothing = 0
fOptional (Just n) = n

fOrder :: Order -> String
fOrder (LimitOrder i bi shi p q s mq fak) = 
  printf "Limit\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t0" i bi shi p q (fSide s) (fOptional mq) (fFAK fak)
fOrder (IcebergOrder i bi shi p q s mq fak dq ps) = 
  printf "Iceberg\t%d\t%d\t%d\t%d\t%d\t%s\t%d\t%s\t%d" i bi shi p q (fSide s) (fOptional mq) (fFAK fak) ps

fTrade :: Trade -> String
fTrade (Trade p q bid sid _ _ _ _) = printf "\t%d\t%d\t%d\t%d\n" p q bid sid

fTrades :: [Trade] -> String
fTrades ts = foldl (++) (printf "\tTrades %d\n" $ length ts) $ map fTrade ts

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

fResponse :: Response -> String
fResponse (NewOrderRs status ts) =
  printf "NewOrderRs\t%s\n%s" (if status == Accepted then "Accepted" else "Rejected") (fTrades ts)
fResponse (CancelOrderRs status _) =
  printf "CancelOrderRs\t%s\n" (if status == Accepted then "Accepted" else "Rejected")
fResponse (ReplaceOrderRs status _ ts) =
  printf "ReplaceOrderRs\t%s\n%s" (if status == Accepted then "Accepted" else "Rejected") (fTrades ts)
fResponse (SetCreditRs s) =
  printf "SetCreditRs\t%s\n" (if s then "Successful" else "Failed") 
fResponse (SetOwnershipRs s) =
  printf "SetOwnershipRs\t%s\n" (if s then "Successful" else "Failed")

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