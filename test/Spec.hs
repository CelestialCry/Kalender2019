import Criterion.Main
import Lib

import LukeFem

main :: IO ()
main = defaultMain [bench "En" $ whnfIO printLøsningFem]
