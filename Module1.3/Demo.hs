{-# LANGUAGE FlexibleContexts #-}
module Demo where

import Text.Parsec
import Control.Applicative

vowel :: Parsec [Char] u Char
vowel = oneOf "aeiou"

getList :: Parsec String u [String]
getList = (many1 digit) `sepBy` (char ';')

p0 :: Parsec [Char] u ([Char], [Char])
p0 = pure (,) <*> many1 letter <*> many1 digit

p1 :: Parsec [Char] u ([Char], [Char])
--p1 = pure (,) <*> many1 letter <*> (many1 space *> many1 digit)
p1 = pure (,) <*> many1 letter <* many1 space <*> many1 digit

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces openBraces closeBraces content = openBraces *> content <* closeBraces

test = ignoreBraces (string "[[") (string "]]") (many1 letter)