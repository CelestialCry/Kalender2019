{-#LANGUAGE OverloadedStrings#-}
module LukeEn (printLøsningEn) where

import qualified Data.Text as T
import System.Directory

infixr 2 £ 
(£) = ($)

-- This is a bit stupid, but it does exactly what I want it to do 
-- if I run the program from the app or src folder
getFile :: IO FilePath
getFile = getCurrentDirectory
    >>= pure . (drop 1) . T.unpack . (foldl (\str acc -> str <> "\\" <> acc ) "") . (<> [filename]) . (takeWhile (/= "Kodekalender")) . (T.splitOn "\\") . T.pack
    where filename = "Kodekalender\\Filer\\Drageproblemer.txt"

parseFile :: FilePath -> IO [Int]
parseFile path = do
    contents <- (T.splitOn ",") . T.strip . T.pack <$> readFile path
    pure $ read . T.unpack <$> contents

data Humør = Mett | LittSulten | Sulten | VeldigSulten | Berserk | Dommedag deriving(Show, Read)
data Drage = Drage {humør :: Humør, størrelse :: Int, rester :: Sauer} deriving(Show, Read)

type Sauer = Int
type Dager = Int

mink :: Humør -> Humør
mink Mett = LittSulten
mink LittSulten = Sulten
mink Sulten = VeldigSulten
mink VeldigSulten = Berserk
mink Berserk = Dommedag

spis :: Sauer -> Dager -> Drage -> (Drage, Dager)
spis s u d
    | mat < spist = (Drage {humør = mink $ humør d, størrelse = spist - 1, rester = 0}, u+1)
    | otherwise = (Drage {humør = Mett, størrelse = spist + 1, rester = mat - spist}, u+1)
    where mat = s + rester d
          spist = størrelse d

lagDrage :: Drage
lagDrage = Drage {humør=Mett, størrelse=50, rester = 0}

erLevende :: Drage -> Bool
erLevende Drage {humør=Dommedag, størrelse = _, rester = _} = False
erLevende d = True

levDag :: [Sauer] -> (Drage, Dager) -> Dager
levDag [] _ = error "Bitch"
levDag (l:ls) (drage, dag) 
    | erLevende drage = levDag ls $ spis l dag drage
    | otherwise = dag-1

printLøsningEn :: IO ()
printLøsningEn = getFile >>= parseFile >>= \sauer -> pure £ levDag sauer (lagDrage,0)
    >>= putStrLn . show 