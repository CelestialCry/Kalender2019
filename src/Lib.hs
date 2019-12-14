{-#LANGUAGE LambdaCase#-}
module Lib ( 
    velgLuke,
--    module LukeTretten
    ) where

import LukeEn
import LukeTo
import LukeTre
import LukeFire
import LukeFem
import LukeSeks
import LukeSyv
import LukeOtte
import LukeNi
import LukeTi
import LukeElleve
import LukeTolv
import LukeTretten

infixr 2 £
(£) = ($)

velgLuke :: IO ()
velgLuke = putStrLn "Velg en luke"
    >> getLine
    >>= \choice -> pure £ (read choice :: Int)
    >>= \case
        1 -> printLøsningEn
        2 -> printLøsningTo
        3 -> printLøsningTre
        4 -> printLøsningFire
        5 -> printLøsningFem
        6 -> printLøsningSeks
        7 -> printLøsningSyv
        8 -> printLøsningÅtte
        9 -> printLøsningNi
        10 -> printLøsningTi
        11 -> printLøsningElleve
        12 -> printLøsningTolv
        13 -> printLøsningTretten
        x -> error "Dette er ennå ikke et feature av mitt program"