{-#LANGUAGE LambdaCase#-}
module Lib
    ( velgLuke
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
        x -> error "Dette er ennå ikke et feature av mitt program"