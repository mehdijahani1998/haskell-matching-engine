module Main where
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.Set                  as Set
import           System.Environment
import           System.IO
import           System.Random
import           Text.Printf

import           Domain.ME
import           Infra.Shahlaa


wellFormed :: Side -> OrderQueue -> Bool
wellFormed Sell os = wellOrdered (<=) os
wellFormed Buy os  = wellOrdered (>=) os


wellOrdered :: (Int -> Int -> Bool) -> OrderQueue -> Bool
wellOrdered _ [] = True
wellOrdered _ [_] = True
wellOrdered cmp (o1:o2:os)
  | (price o1) `cmp` (price o2) = wellOrdered cmp (o2:os)
  | otherwise = False


type RawOrder = (Price, Quantity, Quantity, Quantity)


genAllPQ :: Price -> Quantity -> [RawOrder]
genAllPQ maxPrice maxQty =
  [(p, q, ma, ps) | p <- [1..maxPrice], q <- [1..maxQty], ma <- [0..q], ps <- [0..q]]


canPrepend :: RawOrder -> [RawOrder] -> Bool
canPrepend o []                           = True
canPrepend (p, _, _, _) ((p1, _, _, _):_) = (p <= p1)


prependToAll :: RawOrder -> [[RawOrder]] -> [[RawOrder]]
prependToAll order queueList =
  foldl (\acc queue -> if (canPrepend order queue) then (acc ++ [order:queue]) else acc)
  []
  queueList


prependAllToAll :: [RawOrder] -> [[RawOrder]] -> [[RawOrder]]
prependAllToAll orderList queueList = foldl (\acc order -> acc ++ (prependToAll order queueList)) [] orderList


genAllPQLists :: Price -> Quantity -> Int -> [[RawOrder]]
genAllPQLists _ _ 0 = [[]]
genAllPQLists maxPrice maxQty n =
  prependAllToAll (genAllPQ maxPrice maxQty) (genAllPQLists maxPrice maxQty (n-1))


makeMinQty :: Quantity -> Maybe Quantity
makeMinQty 0 = Nothing
makeMinQty n = Just n


makeOrder :: Int -> Side -> RawOrder -> Order
makeOrder i s (p, q, mq, 0) =
  limitOrder i p q s (makeMinQty mq)
makeOrder i s (p, q, mq, ps) =
  icebergOrder i p q s (makeMinQty mq) ps ps


makeOrderFormAll :: [RawOrder] -> [Order]
makeOrderFormAll list = zipWith (\(p, q, mq, ps) i -> makeOrder i Sell (p, q, mq, ps)) list [0..]


genAllSellQueues :: Price -> Quantity -> Int -> [OrderQueue]
genAllSellQueues maxPrice maxQty n =
  filter (wellFormed Sell) $
  map makeOrderFormAll $
  genAllPQLists maxPrice maxQty n
  where


genAllB1SnInputs :: Price -> Quantity -> Int -> [[Order]]
genAllB1SnInputs maxPrice maxQty queueLen =
  [ sq ++ [b] | b <- map (makeOrder (maxPrice * maxQty) Buy) (genAllPQ maxPrice maxQty),
                sq <- genAllSellQueues maxPrice maxQty queueLen]


genAllB1SnTestSuite :: Price -> Quantity -> Int -> [TestCase]
genAllB1SnTestSuite maxPrice maxQty queueLen = map addOracle (genAllB1SnInputs maxPrice maxQty queueLen)


-- ---------------------------------------------------------------


allSellOrders = genAllPQ


canAppend :: RawOrder -> [RawOrder] -> Bool
canAppend o []            = True
canAppend (p, _, _, _) os = (p >= p1) where (p1, _, _, _) = last os


appendToAll :: RawOrder -> [[RawOrder]] -> [[RawOrder]]
appendToAll order queueList =
  foldl (\acc queue -> if (canAppend order queue) then (acc ++ [queue ++ [order]]) else acc)
  []
  queueList


appendAllToAll :: [RawOrder] -> [[RawOrder]] -> [[RawOrder]]
appendAllToAll orderList queueList =
  foldl (\acc order -> acc ++ (appendToAll order queueList)) [] orderList


gen :: Int -> Quantity -> Order -> [[RawOrder]] -> [RawOrder] -> [[RawOrder]]
gen maxLength maxQty buyOrder curSuite curQueue =
  gen' maxLength 0 buyOrder (allSellOrders (price buyOrder) maxQty) curSuite curQueue


gen' :: Int -> Int -> Order -> [RawOrder] -> [[RawOrder]] -> [RawOrder] -> [[RawOrder]]
gen' maxLength curLength buyOrder allSellOrders curSuite curQueue
  | curLength >= maxLength = curSuite ++ [curQueue]
  | completeMatch buyOrder curQueue = curSuite ++ [curQueue]
  | otherwise =
      foldl
        (\acc ts -> acc ++ ts)
        curSuite
        (map (gen' maxLength (curLength+1) buyOrder allSellOrders curSuite) (appendAllToAll allSellOrders [curQueue]))


completeMatch buyOrder curQueue =
  let
    (rem, queue, ts) = evalState (matchBuy buyOrder (map (makeOrder 0 Sell) curQueue)) []
  in
    rem == Nothing


buySellQueuePairs maxLength maxPrice maxQuantity =
  [(b, sq) | b <- (map (makeOrder 0 Buy) (genAllPQ maxPrice maxQuantity)), sq <- (gen maxLength maxQuantity b [] [])]


gen2 maxLength maxPrice maxQuantity =
  foldl (++) [] $
  map
    (\(b, sq) -> [(map (makeOrder 0 Sell) sq) ++ [b]])
    (buySellQueuePairs maxLength maxPrice maxQuantity)


main = do
  args <- getArgs
  let queueLen = read (args !! 0) :: Int
  let maxPrice = read (args !! 1) :: Int
  let maxQty = read (args !! 2) :: Int
  hPutStrLn stderr "Generating Input"
  let inp = gen2 queueLen maxPrice maxQty
  hPutStrLn stderr "Adding Oracle"
  let ts = map addOracle inp
  hPutStrLn stderr "Writing into File"
  -- print $ length ts
  -- putStrLn ""
  mapM_ (\tc -> putStrLn $ fTestCase tc) ts


