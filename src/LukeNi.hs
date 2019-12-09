{-#LANGUAGE OverloadedStrings, LambdaCase#-}
module LukeNi where

import qualified Data.Text as T
import System.Directory
import Data.Foldable (foldl')


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
    where filename = "Kodekalender\\Filer\\krampus.txt"

parseFile :: FilePath -> IO [Integer]
parseFile path = readFile path >>= \str -> pure $ read . T.unpack <$> ((T.splitOn "\n") . T.pack $ str)

maybeEq :: Integer -> Maybe Integer -> Bool
maybeEq _ Nothing = False
maybeEq a (Just b) = a==b

isKrampus :: Integer -> Bool
isKrampus int = foldr (\x acc -> acc || (maybeEq int x)) False $ reduce . pair <$> (take tens [10^x | x<-[1..]])
    where tens = length . show $ int^2
          pair = \x -> (div (int^2) x, mod (int^2) x)
          reduce = \(a,b) -> \case 
                                0 -> Nothing
                                x -> \case
                                        0 -> Nothing
                                        y -> Just $ x+y
                                        $ a
                                $ b

printLøsningNi :: IO ()
printLøsningNi = getFile >>= parseFile >>= \ls -> putStrLn . show $ foldr (+) 0 $ filter isKrampus ls