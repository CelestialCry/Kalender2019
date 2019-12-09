import Criterion.Main
import Lib

import LukeNi

main :: IO ()
main = defaultMain [bench "Ni" $ whnfIO printLøsningNi]
