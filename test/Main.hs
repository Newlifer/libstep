{-# LANGUAGE OverloadedStrings #-}
import Step
import Data.Attoparsec.ByteString.Char8

main :: IO ()
main = print $ parseOnly parseVector "(  1.45  , 666.    ,2.   ,6.022E23)"
{-main = print $ parseOnly parseVector "(1.0,2.0,3.0,4.0)"-}
