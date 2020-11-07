module Shahlaa where
import qualified Data.Set as Set -- From the 'containers' library
import Text.Printf
import System.Random
import Control.Monad
import Control.Monad.Trans.State
import Coverage
import ME

data TestCase = TestCase {
    input :: [Order]
  , output :: [Trade]
  , coverage :: [CoverageInfo]
  } deriving (Eq, Show)

type SysState = (OrderBook, [Trade], [CoverageInfo])
initSysState :: SysState
initSysState = (OrderBook [] [], [], [])

processOrder :: SysState -> Order -> SysState
processOrder (ob, ts, covs) o =
  let 
    ((ob', ts'), cov) = runState (handleNewOrder o ob) noCoverage
  in
    (ob', ts ++ ts', covs ++ [cov])

addOracle :: [Order] -> TestCase
addOracle os = 
  let 
    (ob, ts, covs) = foldl processOrder initSysState os
  in
    TestCase os ts covs

jSide :: Side -> String
jSide Sell = "SELL"
jSide Buy = "BUY "

jMinQty :: Maybe Quantity -> String
jMinQty Nothing = ".minQty(Optional.empty())"
jMinQty (Just q') = printf ".minQty(Optional.of(%d))" q'

jOrder :: Order -> String
jOrder (LimitOrder i p q s mq) = 
  printf "\tShahlaaOrder.builder().id(%d).price(%d).qty(%d).side(OrderSide.%s)%s.disclosedQty(Optional.empty()).build(),\n" i p q (jSide s) (jMinQty mq)
jOrder (IcebergOrder i p q s mq dq ps) = 
  printf "\tShahlaaOrder.builder().id(%d).price(%d).qty(%d).side(OrderSide.%s)%s.disclosedQty(Optional.of(%d)).build(),\n" i p q (jSide s) (jMinQty mq) ps

fOptional :: Maybe Int -> Int
fOptional Nothing = 0
fOptional (Just n) = n

fOrder :: Order -> String
fOrder (LimitOrder i p q s mq) = 
  printf "%d\t%d\t%d\t%s\t%d\t0\n" i p q (jSide s) (fOptional mq)
fOrder (IcebergOrder i p q s mq dq ps) = 
  printf "%d\t%d\t%d\t%s\t%d\t%d\n" i p q (jSide s) (fOptional mq) ps

jTrade :: Trade -> String
jTrade (Trade p q bid sid) = printf "\tnew ShahlaaTrade(%d,%d,%d,%d),\n" p q bid sid

fTrade :: Trade -> String
fTrade (Trade p q bid sid) = printf "%d\t%d\t%d\t%d\n" p q bid sid

jInput :: [Order] -> String
jInput q = (foldl (++) "new ShahlaaOrder[] {\n" $ map jOrder q) ++ "}, "

fInput :: [Order] -> String
fInput q = foldl (++) (printf "%d\n" $ length q) $ map fOrder q

jOutput :: [Trade] -> String
jOutput ts = (foldl (++) "new ShahlaaTrade[] {\n" $ map jTrade ts) ++ "}"

fOutput :: [Trade] -> String
fOutput ts = foldl (++) (printf "%d\n" $ length ts) $ map fTrade ts

jTestCase :: TestCase -> String
jTestCase (TestCase inp out _) = "runTC(" ++ (jInput inp) ++ (jOutput out) ++ ");\n"

fTestCase :: TestCase -> String
fTestCase (TestCase inp out _) = (fInput inp) ++ (fOutput out) ++ "\n"

jTestSuite :: [TestCase] -> String
jTestSuite = foldl (\acc tc -> acc ++ (jTestCase tc) ++ "\n") ""

fTestSuite :: [TestCase] -> String
fTestSuite ts = foldl (\acc tc -> acc ++ (fTestCase tc) ++ "\n") (printf "%d\n" $ length ts) ts

-- coverageScoreTC :: TestCase -> Int
-- coverageScoreTC = coverageScore . coverage

-- avgCoverageScoreTS :: [TestCase] -> Double
-- avgCoverageScoreTS ts = (fromIntegral $ sum $ map coverageScoreTC ts) / (fromIntegral $ length ts)