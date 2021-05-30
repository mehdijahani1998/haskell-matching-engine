module Main where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Printf
import System.Random
import ME
import MEService
import Shahlaa


data TCGenSpec = TCGenSpec 
  { brokerCount :: Int
  , minCredit :: Int
  , maxCredit :: Int
  , shareholderCount :: Int
  , minPercentage :: Int
  , maxPercentage :: Int
  , maxPrice :: Int
  , maxQty :: Int
  , orderCount :: Int
  , orderPercentageWithMinQty :: Int
  , icebergPercentage :: Int
  , fakPercentage :: Int
  } deriving (Show, Eq)


genRandSetCreditRq :: BrokerID -> TCGenSpec -> IO Request
genRandSetCreditRq brokerID g = 
  do { credit <- randomRIO (minCredit g, maxCredit g)
     ; return (SetCreditRq brokerID credit)
     }


genRandSetCreditRqs :: TCGenSpec -> IO [Request]
genRandSetCreditRqs g = 
  sequence [ genRandSetCreditRq i g | i <- [1..(brokerCount g)] ]


genRandSetOwnershipRq :: ShareholderID -> TCGenSpec -> IO Request
genRandSetOwnershipRq shareholderID g = 
  do { percentage <- randomRIO (minPercentage g, maxPercentage g)
     ; return (SetOwnershipRq shareholderID percentage)
     }


genRandSetOwnershipRqs :: TCGenSpec -> IO [Request]
genRandSetOwnershipRqs g = 
  sequence [ genRandSetOwnershipRq i g | i <- [1..(shareholderCount g)] ]

genRandLimitOrder :: OrderID -> TCGenSpec -> IO Order
genRandLimitOrder newoid g = 
  do { s <- randomIO :: IO Bool
     ; p <- randomRIO (1, maxPrice g)
     ; q <- randomRIO (1, maxQty g)
     ; mqPerc <- randomRIO (0, 99)
     ; fakPerc <- randomRIO (0, 99)
     ; m <- randomRIO (1, q)
     ; bi <- randomRIO (1, brokerCount g)
     ; shi <- randomRIO (1, shareholderCount g)
     ; return (
        let hasMQ = mqPerc < (orderPercentageWithMinQty g) in
          limitOrder newoid bi shi p q (if s then Buy else Sell) 
                     (if hasMQ then Just m else Nothing) 
                     ((not hasMQ) && (fakPerc < (fakPercentage g)))
        )
  }


genRandIcebergOrder :: OrderID -> TCGenSpec -> IO Order
genRandIcebergOrder newoid g = 
  do { s <- randomIO :: IO Bool
     ; p <- randomRIO (1, maxPrice g)
     ; q <- randomRIO (1, maxQty g)
     ; mqPerc <- randomRIO (0, 99)
     ; m <- randomRIO (1, q)
     ; bi <- randomRIO (1, brokerCount g)
     ; shi <- randomRIO (1, shareholderCount g)
     ; ps <- randomRIO (1, max 1 (q `div` 2))
     ; return (icebergOrder newoid bi shi p q (if s then Buy else Sell) (if mqPerc <= (orderPercentageWithMinQty g) then Just m else Nothing) False ps ps)
  }


genRandOrder :: OrderID -> TCGenSpec -> IO Order
genRandOrder newoid g =
  do { icebergPerc <- randomRIO (0, 100)
     ; if icebergPerc < (icebergPercentage g) then 
         genRandIcebergOrder newoid g
       else
         genRandLimitOrder newoid g
  }


genRandNewOrderRq :: OrderID -> TCGenSpec -> IO Request
genRandNewOrderRq newoid g = do 
  { o <- genRandOrder newoid g 
  ; return (NewOrderRq o)
  }


genRandNewOrderRqs :: OrderID -> TCGenSpec -> IO [Request]
genRandNewOrderRqs newoid g =
  sequence $ take (orderCount g) [genRandNewOrderRq i g | i <- [newoid..]]

genRandTestCase :: TCGenSpec -> IO TestCase
genRandTestCase g =
  do { setCreditRqs <- genRandSetCreditRqs g 
     ; setOwnershipRqs <- genRandSetOwnershipRqs g
     ; newOrderRqs <- genRandNewOrderRqs 1 g
     ; return (addOracle (setCreditRqs ++ setOwnershipRqs ++ newOrderRqs))
  }


genRandTestSuite :: Int -> TCGenSpec -> IO [TestCase]
genRandTestSuite testSuiteSize g =
  sequence $ [ genRandTestCase g | i <- [0..(testSuiteSize-1)]]


main = do
  setStdGen (mkStdGen 42)   -- optional
  o <- genRandTestSuite 1 -- number of test cases in the suite
    TCGenSpec {
      brokerCount = 5,
      minCredit = 50,
      maxCredit = 200,
      shareholderCount = 5,
      minPercentage = 5,
      maxPercentage = 20,
      maxPrice = 10,
      maxQty = 10,
      orderCount = 100,
      orderPercentageWithMinQty = 30,
      icebergPercentage = 30,
      fakPercentage = 20
    }
  -- mapM (print . coverage) o
  -- mapM (print . Set.fromList . coverage) o
  -- mapM (print . coverageScoreTC) o
  putStrLn $ fTestSuite o
