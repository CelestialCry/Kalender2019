module LukeSyv (printLøsningSyv, γ) where

import Data.Numbers.Primes

φ :: Integral int => int -> int
φ i = foldr (*) 1 $ (+(-1)) <$> primeFactors i

modInv :: Integral int => int -> int -> int
modInv a n = flip mod n $ a^(φ n - 1)

γ :: Integral a => a -> a -> a -> a
γ a x n = flip mod n $ a * modInv x n

printLøsningSyv :: IO ()
printLøsningSyv = putStrLn . show $ γ 5897 7 27644437