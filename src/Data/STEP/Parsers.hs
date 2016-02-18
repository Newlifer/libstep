{-# LANGUAGE OverloadedStrings #-}
module Data.STEP.Parsers (
  Vector(..)

, parseStep
) where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8

data Vector = Vector [Double]
  deriving (Show, Eq)

delimiter :: Char
delimiter = ','

groupStart :: Char
groupStart = '('

groupFinish :: Char
groupFinish = ')'

spaceSkip :: Parser [Char]
spaceSkip = many' $ satisfy $ inClass [' ', '\t']

parseVectorElement :: Parser Double
parseVectorElement = do
    void $ spaceSkip
    x <- double
    void $ spaceSkip
    return $ x

parseVector :: Parser Vector
parseVector = do
    void $ char groupStart
    result <- parseVectorElement `sepBy` (char delimiter)
    void $ char groupFinish
    return $ Vector result

parseStep :: Parser Vector
parseStep = parseVector
