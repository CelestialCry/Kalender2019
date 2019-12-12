module LukeTolv (printLøsningTolv) where

import Data.List (sort)

newInt :: Int -> Int
newInt i = big - small
    where str = sort . (\ls -> (take (4 - length ls) ['0','0'..]) <> ls) . show $ i 
          small = read str
          big = read . reverse $ str :: Int

runIt :: (Int,Int) -> Int
runIt (i,n)
    | i == 6174 = n
    | i == 0 = 0
    | otherwise = runIt (newInt i, n+1)

printLøsningTolv :: IO ()
printLøsningTolv = putStrLn . show . length $ filter (==7) $ map runIt $ zip [1000..9999] [0,0..]