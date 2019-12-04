{-#LANGUAGE OverloadedStrings#-}
module LukeTo (printLøsningTo) where

import qualified Data.Text as T
import System.Directory

getFile :: IO FilePath
getFile = getCurrentDirectory
    >>= pure . (drop 1) . T.unpack . (foldl (\str acc -> str <> "\\" <> acc ) "") . (<> [filename]) . (takeWhile (/= "Kodekalender")) . (T.splitOn "\\") . T.pack
    where filename = "Kodekalender\\Filer\\World.txt"

parseFile :: IO [String]
parseFile = getFile >>= readFile >>= pure . (map T.unpack) . (T.splitOn "\n"). T.pack

getAns :: String -> Int
getAns l = (foldr (\ls acc -> acc + length ls) 0) . (map T.unpack) . init . tail $ T.splitOn "#" $ T.pack l

printLøsningTo :: IO ()
printLøsningTo = parseFile >>= \ls -> putStrLn . show $ foldr (+) 0 $ getAns <$> ls