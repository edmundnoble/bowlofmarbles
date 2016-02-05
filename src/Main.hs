import Control.Monad.State.Strict
import System.Random
import Data.Maybe
import Control.Arrow
import Data.List
import Data.HashMap.Strict (HashMap,fromList,foldrWithKey,adjust)
import Criterion.Main
import Criterion.Main.Options
import Criterion.Types
import Control.DeepSeq

type Color = Int

data Bowl = Bowl { candies :: HashMap Color Int
                 , howManyCandies :: Int }

{-# INLINE bowlFromList #-}
bowlFromList :: [(Color, Int)] -> Bowl
bowlFromList = makeBowl . fromList

{-# INLINE CONLIKE makeBowl #-}
makeBowl :: HashMap Color Int -> Bowl
makeBowl m = Bowl { candies = m, howManyCandies = sum $! m }

pickNFromBowl :: Int -> Bowl -> Color
pickNFromBowl n b = (fromJust . fst) $ foldrWithKey (\c i t@(mc, n) -> case mc of
    Just _ -> t
    Nothing -> if n < i then (Just c, n) else (mc, n - i)
    ) (Nothing, n) (candies b)

pick :: RandomGen g => State (g, Bowl) Color
pick = do
  (r, firstBowl) <- get
  let (idx, r') = randomR (0, howManyCandies firstBowl - 1) r
      picked = pickNFromBowl idx firstBowl
      newCandies = adjust (\i -> i - 1) picked (candies firstBowl)
  put (r', Bowl { candies = newCandies, howManyCandies = howManyCandies firstBowl - 1 })
  return picked

{-# INLINE press #-}
press :: NFData a => a -> Benchmarkable
press = nf rnf

main = do
  r <- getStdGen
  let bowl = bowlFromList [(1, 20000), (2, 40000), (3, 40000), (4, 20000)]
  defaultMainWith (defaultConfig { reportFile = Just "report" }) [ bench "120000 picks" . press $ evalState (replicateM 120000 pick) (r, bowl) ]
