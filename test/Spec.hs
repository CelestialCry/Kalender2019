import Criterion.Main
import Lib

import LukeEn

main :: IO ()
main = defaultMain [bench "En" $ whnfIO printLøsningEn]
