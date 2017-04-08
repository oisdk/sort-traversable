import           Criterion.Main
import           System.Random

import           Data.Traversable.Sort.PairingHeap (sortTraversable)
import           Data.List         (sort)

import qualified Data.Sequence     as Seq

import           Control.Monad     (replicateM)

randInt :: IO Int
randInt = randomIO

testAtSize :: Int -> Benchmark
testAtSize n =
    bgroup
        (show n)
        [ env (replicateM n randInt) $
          \lst ->
               bgroup
                   "list"
                   [ bench "Data.List" $ nf sort lst
                   , bench "HSTrav" $ nf sortTraversable lst]
        , env (Seq.replicateM n randInt) $
          \sq ->
               bgroup
                   "sequence"
                   [ bench "sort" $ nf Seq.unstableSort sq
                   , bench "unstableSort" $ nf Seq.sort sq
                   , bench "HSTrav" $ nf sortTraversable sq]]

main :: IO ()
main = defaultMain $ map testAtSize [5000, 10000, 20000, 1000000]

