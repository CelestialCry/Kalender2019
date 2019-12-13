{-#LANGUAGE OverloadedStrings#-}
module LukeTretten where

import qualified Data.Text as T
import System.Directory
import Data.Aeson

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
    where filename = "Kodekalender\\Filer\\lab.txt"

parseFile :: FilePath -> IO (Maybe [[String]])
parseFile path = decodeFileStrict path 