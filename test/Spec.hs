import Criterion.Main
import Lib

import LukeOtte

main :: IO ()
main = defaultMain [bench "Åtte" $ whnfIO printLøsningÅtte]
