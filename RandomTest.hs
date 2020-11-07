module Main where
import qualified Data.Set as Set
import Text.Printf
import System.Random
import ME
import Shahlaa

genRandLimitOrder :: OrderID -> Price -> Quantity -> IO Order
genRandLimitOrder newoid maxPrice maxQty = 
  do { s <- randomIO :: IO Bool
     ; p <- randomRIO (1, maxPrice)
     ; q <- randomRIO (1, maxQty)
     ; hasMQ <- randomIO :: IO Bool
     ; m <- randomRIO (1, q)
     ; return (limitOrder newoid p q (if s then Buy else Sell) (if hasMQ then Just m else Nothing))
  }

genRandIcebergOrder :: OrderID -> Price -> Quantity -> IO Order
genRandIcebergOrder newoid maxPrice maxQty = 
  do { s <- randomIO :: IO Bool
     ; p <- randomRIO (1, maxPrice)
     ; q <- randomRIO (1, maxQty)
     ; hasMQ <- randomIO :: IO Bool
     ; m <- randomRIO (1, q)
     ; ps <- randomRIO (1, q)
     ; return (icebergOrder newoid p q (if s then Buy else Sell) (if hasMQ then Just m else Nothing) ps ps)
  }

genRandOrder :: OrderID -> Price -> Quantity -> IO Order
genRandOrder newoid maxPrice maxQty =
  do { isIceberg <- randomIO :: IO Bool
     ; if isIceberg then 
         genRandIcebergOrder newoid maxPrice maxQty
       else
         genRandLimitOrder newoid maxPrice maxQty
  }

genRandOrders :: OrderID -> Price -> Quantity -> Int -> IO [Order]
genRandOrders newoid maxPrice maxQty n =
  sequence $ take n [genRandOrder i maxPrice maxQty | i <- [newoid..]]

genRandTestCase :: Price -> Quantity -> Int -> IO TestCase
genRandTestCase maxPrice maxQty n = 
  do { inp <- genRandOrders 1 maxPrice maxQty n
     ; return (addOracle inp)
  }
    
genRandTestSuite :: Price -> Quantity -> Int -> Int -> IO [TestCase]
genRandTestSuite maxPrice maxQty tcSize tsSize =
  sequence $ [genRandTestCase maxPrice maxQty tcSize | i <- [0..(tsSize-1)]]

main = do
  setStdGen (mkStdGen 42)   -- optional
  o <- genRandTestSuite 10 10 10 5
  -- mapM (print . coverage) o
  -- mapM (print . Set.fromList . coverage) o
  -- mapM (print . coverageScoreTC) o
  putStrLn $ fTestSuite o
