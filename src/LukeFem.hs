{-#LANGUAGE OverloadedStrings#-}
module LukeFem (printLøsningFem) where

import qualified Data.Text as T

scrambled :: T.Text
scrambled =  "tMlsioaplnKlflgiruKanliaebeLlkslikkpnerikTasatamkDpsdakeraBeIdaegptnuaKtmteorpuTaTtbtsesOHXxonibmksekaaoaKtrssegnveinRedlkkkroeekVtkekymmlooLnanoKtlstoepHrpeutdynfSneloietbol"

test :: T.Text
test = "oepHlpslainttnotePmseormoTtlst"

unSlice :: T.Text -> T.Text
unSlice text = foldr (<>) "" $ reverse $ T.chunksOf half text
    where half = flip div 2 $ T.length text

unShuffle :: T.Text -> T.Text
unShuffle txt = foldr (<>) "" $ T.reverse <$> T.chunksOf 2 txt

unMix :: T.Text -> T.Text
unMix text = foldr (<>) "" $ reverse $ T.chunksOf 3 text

unScramble :: T.Text -> T.Text
unScramble = unSlice . unShuffle . unMix 

printLøsningFem :: IO ()
printLøsningFem = putStrLn . show . unScramble $ scrambled