{-#LANGUAGE OverloadedStrings#-}
module LukeElleve where

import qualified Data.Text as T
import System.Directory

import Prelude hiding (init)

getFile :: IO FilePath
getFile = 
    getCurrentDirectory
    >>= pure 
        . (drop 1) 
        . T.unpack 
        . (foldl (\str acc -> str <> "\\" <> acc ) "") 
        . (<> [filename]) 
        . (takeWhile (/= "Kodekalender")) 
        . (T.splitOn "\\") 
        . T.pack
    where filename = "Kodekalender\\Filer\\terreng.txt"

parseFile :: String -> IO [Terreng]
parseFile path = readFile path >>= \str -> pure $ read . pure <$> str 

data Terreng = G | I | A | S | F deriving(Show, Read, Eq)

data Slede = Sl {
    fart :: Int,
    stack :: Int,
    fjell :: Bool,
    lengde :: Int
} deriving(Show)

init :: Slede
init = Sl {
    fart = 10703437,
    stack = 1,
    fjell = False,
    lengde = 0
}

skli :: Terreng -> Slede -> Slede
skli G s = Sl {
    fart = fart s - 27,
    stack = 1,
    fjell = False,
    lengde = lengde s + 1
    }
skli I s = Sl {
    fart = fart s + 12*(stack s),
    stack = stack s + 1,
    fjell = False,
    lengde = lengde s + 1
    } 
skli A s = Sl {
    fart = fart s - 59,
    stack = 1,
    fjell = False,
    lengde = lengde s + 1
    }
skli S s = Sl {
    fart = fart s - 212,
    stack = 1,
    fjell = False,
    lengde = lengde s + 1
    }
skli F s 
    | fjell s = Sl {
        fart = fart s + 35,
        stack = 1,
        fjell = False,
        lengde = lengde s + 1
        }
    | otherwise = Sl {
        fart = fart s - 70,
        stack = 1,
        fjell = True,
        lengde = lengde s + 1
        }

skliOrNotSkli :: Slede -> Terreng -> Slede
skliOrNotSkli s t
    | fart s <= 0 = s
    | otherwise = skli t s

printLøsningElleve :: IO ()
printLøsningElleve = getFile 
    >>= parseFile 
    >>= putStrLn . show . (foldl skliOrNotSkli init)