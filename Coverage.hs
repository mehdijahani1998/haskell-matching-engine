module Coverage where
import Control.Monad.Trans.State
import Data.Set as Set -- From the 'containers' library

type CoverageItem = String
type CoverageInfo = [CoverageItem]

noCoverage :: CoverageInfo
noCoverage = []

coverageScore :: CoverageInfo -> Int
coverageScore = Set.size . Set.fromList

type Coverage = State CoverageInfo

covers :: a -> CoverageItem -> Coverage a
covers value item = do 
  curCoverage <- get
  put (item:curCoverage)
  return value  