import Criterion.Main
import Lib

import LukeFjorten

main :: IO ()
main = defaultMain [bench "test1" $ whnf test 100]
