{-# LANGUAGE OverloadedStrings #-}
module Step (
  Vector(..)

, parseStep
) where

import Data.Attoparsec.ByteString.Char8
import Data.Word

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

sepParser :: Parser [Char]
sepParser = spaceSkip >> char delimiter >> spaceSkip

parseVectorElement :: Parser Double
parseVectorElement = do
    spaceSkip
    x <- double
    spaceSkip
    return $ x

parseVector :: Parser Vector
parseVector = do
    char groupStart
    result <- parseVectorElement `sepBy` (char delimiter)
    char groupFinish
    return $ Vector result

parseStep :: Parser Vector
parseStep = parseVector
