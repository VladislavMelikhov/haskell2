module Demo where

import Data.Functor
import Control.Applicative
import Control.Monad

{-

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

class (Alternative m, Monad m) => MonadPlus m where
    mzero :: m a
    mzero = empty
    mplus :: m a -> m a -> m a
    mplus = (<|>)



instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

instance MonadPlus Maybe



instance Alternative [] where
    empty = []
    (<|>) = (++)

instance MonadPlus []

-}

{-

Законы для класса типов MonadPlus:

I. Left Zero
mzero >>= k == mzero

для Applicative было:
empty <*> a == empty

однако из этого не следует выполнение I.

II. Right Zero
v >> mzero == mzero


Для представителей MonadPlus должен выполняться хотя бы один из законов III. и IV.

III. Left Distribution
(a `mplus` b) >>= k == (a >>= k) `mplus` (b >>= k)

IV. Left Catch Low
return a `mplus` b == return a

-}

newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP predicate = PrsEP parse
    where
        parse :: Int -> String -> (Int, Either String (Char, String))
        parse position (x:xs) | predicate x = (position + 1, Right (x, xs)) 
                              | otherwise   = failure position [x]
        parse position []                   = failure position "end of input"

        failure :: Int -> String -> (Int, Either String (Char, String))
        failure position cause = (nextPosition, Left ("pos " ++ show nextPosition ++ ": unexpected " ++ cause))
            where
                nextPosition = position + 1

instance Functor PrsEP where
  --fmap :: (a -> b) -> PrsEP a -> PrsEP b
    fmap f (PrsEP parseA) = PrsEP parseB 
        where
            parseB position str = (liftF f) <$> (parseA position str) 

            liftF :: (a -> b) -> Either String (a, String) -> Either String (b, String)
            liftF func containerA = do 
                (a, str') <- containerA
                return (func a, str')

instance Applicative PrsEP where
  --pure :: a -> PrsEP a
    pure a = PrsEP parse
        where
            parse position str = (position, Right (a, str))

  --(<*>) :: PrsEP (a -> b) -> PrsEP a -> PrsEP b
    (<*>) (PrsEP parseF) (PrsEP parseA) = PrsEP parseB
        where
            parseB position str = 
            