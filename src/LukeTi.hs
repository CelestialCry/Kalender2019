{-#LANGUAGE OverloadedStrings#-}
module LukeTi (printLøsningTi) where

import qualified Data.Text as T
import Data.List.Split
import Data.List (sort)
import System.Directory

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
    where filename = "Kodekalender\\Filer\\logg.txt"

parseFile :: String -> IO [(Data, Dag)]
parseFile path = 
    readFile path 
    >>= pure 
        . (\ls -> zip ls $ cycle [Ma, Ti, On, To, Fr, Lø, Sø])
        . init
        . (map $ toData . sort)
        . (map . map $
            (\(a,b) -> (ezRead a, read b)) 
            . (\(a,b) -> (T.unpack a, T.unpack b)) 
            . (\ls -> (ls !! 3, ls !! 1)) 
            . (T.splitOn " ")
            ) 
        . (map $ 
            drop 1
            ) 
        . (chunksOf 4) 
        . (T.splitOn "\n") 
        . T.pack

data Forbruk = Tannkrem | Sjampo | Toalettpapir deriving(Eq, Ord)

instance Show Forbruk where
    show Tannkrem = "tannkrem"
    show Sjampo = "sjampo"
    show Toalettpapir = "toalettpapir"

ezRead :: String -> Forbruk
ezRead "tannkrem" = Tannkrem
ezRead "sjampo" = Sjampo
ezRead "toalettpapir" = Toalettpapir
ezRead _ = error "dette burde ikke være her"

data Data = D {
    tannkrem :: Double,
    sjampo :: Double,
    toalettpapir :: Double
} deriving(Show, Read, Eq)

instance Semigroup Data where
    a <> b = D {
        tannkrem = tannkrem a + tannkrem b,
        sjampo = sjampo a + sjampo b,
        toalettpapir = toalettpapir a + toalettpapir b
    }

instance Monoid Data where
    mempty = D {
        tannkrem = 0,
        sjampo = 0,
        toalettpapir = 0
    }

toData :: [(Forbruk, Double)] -> Data
toData (a:b:c:[]) = D {
    tannkrem = snd a,
    sjampo = snd b,
    toalettpapir = snd c
}
toData _ = error "Du fucka opp parsingen"

data Dag = Ma | Ti | On | To | Fr | Lø | Sø deriving(Show, Read, Eq, Ord)

filterDag :: Either Dag Bool -> [(Data, Dag)] -> [Data]
filterDag (Right True) ls = map fst ls
filterDag (Right False) _ = []
filterDag (Left dag) ls = map fst $ filter (\el -> snd el == dag) ls

printLøsningTi :: IO ()
printLøsningTi = do
    ls <- (getFile >>= parseFile) 
    let tot = mconcat $ filterDag (Right True) ls
    let totTann = (tannkrem tot)/125
    let totSjampo = (sjampo tot)/300
    let totToalett = (toalettpapir tot)/25
    let søSjampo = sjampo . mconcat $ filterDag (Left Sø) ls
    let onToalett = toalettpapir . mconcat $ filterDag (Left On) ls
    putStrLn ("totTann: " <> show totTann)
    putStrLn ("totSjampo: " <> show totSjampo)
    putStrLn ("totToalettpapir: " <> show totToalett)
    putStrLn ("søSjampo: " <> show søSjampo)
    putStrLn ("onToalett: " <> show onToalett)