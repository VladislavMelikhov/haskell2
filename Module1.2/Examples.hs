module Examples where

{-# LANGUAGE RankNTypes#-}
import Control.Applicative ((<**>),ZipList(..))

infixl 4 <*?>
(<*?>) :: Applicative f => f a -> f (a -> b) -> f b
(<*?>) = flip (<*>)

exprMaybe :: (forall a b . Maybe a -> Maybe (a -> b) -> Maybe b) -> Maybe Int
exprMaybe op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Just 5 <??> Just (+2) -- No counterexample

exprList :: (forall a b . [a] -> [a -> b] -> [b]) -> [Int]
exprList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in [1,1] <??> [(+3),(+4)] -- It is counterexample

exprZipList :: (forall a b . ZipList a -> ZipList (a -> b) -> ZipList b) -> ZipList Int
exprZipList op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ZipList [1,2] <??> ZipList [(+3),(+4)]  -- No counterexample

exprEither :: (forall a b . Either String a -> Either String (a -> b) -> Either String b) -> Either String Int
exprEither op = 
  let (<??>) = op 
      infixl 4 <??> 
  in Left "AA" <??> Left "BB"  -- It is counterexample

exprPair :: (forall a b . (String,a) -> (String,a -> b) -> (String,b)) -> (String,Int)
exprPair op = 
  let (<??>) = op 
      infixl 4 <??> 
  in ("3", 3) <??> ("+1",(+1))  -- It is counterexample

exprEnv :: (forall a b . (String -> a) -> (String -> (a -> b)) -> (String -> b)) -> (String -> Int)
exprEnv op = 
  let (<??>) = op 
      infixl 4 <??> 
  in length <??> (\_ -> (+5))  -- No counterexample