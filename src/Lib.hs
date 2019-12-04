{-#LANGUAGE LambdaCase#-}
module Lib
    ( velgLuke
    ) where

import LukeEn
import LukeTo
import LukeTre
import LukeFire

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
        x -> error "Dette er ennå ikke et feature av mitt program"