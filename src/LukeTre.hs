{-#LANGUAGE OverloadedStrings#-}
module LukeTre (printLøsningTre) where

import qualified Data.Text as T
import System.Directory

infixr 2 £
(£) = ($)

getFile :: IO FilePath
getFile = getCurrentDirectory
    >>= pure . (drop 1) . T.unpack . (foldl (\str acc -> str <> "\\" <> acc ) "") . (<> [filename]) . (takeWhile (/= "Kodekalender")) . (T.splitOn "\\") . T.pack
    where filename = "Kodekalender\\Filer\\IKEA.txt"

outFile :: String -> IO FilePath
outFile file = getCurrentDirectory
    >>= pure . (drop 1) . T.unpack . (foldl (\str acc -> str <> "\\" <> acc ) "") . (<> [filename]) . (takeWhile (/= "Kodekalender")) . (T.splitOn "\\") . T.pack
    where filename = "Kodekalender\\Filer\\IKEAout\\" <> T.pack file <> ".txt"

parseFile :: Int -> IO [String]
parseFile i = getFile >>= readFile >>= pure . (slice i)

slice :: Int -> [a] -> [[a]]
slice _ [] = []
slice i ls = (take i ls) : (slice i $ drop i ls)

appendTo :: FilePath -> [String] -> IO ()
appendTo _ [] = pure ()
appendTo path (l:ls) = appendFile path £ l <> "\n" >> appendTo path ls

clearFile :: FilePath -> IO ()
clearFile = flip writeFile ""

divisors :: Int -> [Int]
divisors 0 = [1..]
divisors i = filter (\n -> mod i n == 0) [1..i]

printLøsningTre :: IO ()
printLøsningTre = (sequence $ (\i -> parseFile i
    >>= \ls -> outFile . show £ i
    >>= \path -> clearFile path
    >> appendTo path ls) <$> (filter (\n -> n<858 && n >= 420) $ divisors 720720))
    >> pure ()