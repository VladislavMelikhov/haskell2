module Demo where

import Data.Char
import Control.Applicative hiding (many)

newtype Parser a = Parser { apply :: String  -> [(a, String)] }

parse :: Parser a -> String -> a
parse p = fst . head . apply p

anyChar :: Parser Char
anyChar = Parser f where
    f ""     = []
    f (c:cs) = [(c,cs)]

instance Functor Parser where
--  fmap :: (a -> b) -> Parser a -> Parser b
    fmap f parser = Parser fun where
        fun str = [ (f a, str') | (a, str') <- apply parser str ]



newtype Prs a = Prs { runPrs :: String -> Maybe (a, String) }

instance Functor Prs where
--  fmap :: (a -> b) -> Prs a -> Prs b
    fmap f parser = Prs fun where
        fun str = do 
            (a, str') <- runPrs parser str
            return (f a, str')

satisfyPrs :: (Char -> Bool) -> Prs Char
satisfyPrs predicate = Prs f where
    f ""                   = Nothing
    f (c:cs) | predicate c = Just (c, cs)
             | otherwise   = Nothing

charPrs :: Char -> Prs Char
charPrs c = satisfyPrs (== c)

anyChr :: Prs Char
anyChr = Prs f where
    f ""     = Nothing
    f (c:cs) = Just (c, cs)



instance Applicative Parser where
--  pure :: a -> Parser a
    pure a = Parser fun where
        fun s = [(a, s)]

--  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) pf pv = Parser fun where
        fun s = [ (g a, s'') | (g, s') <- apply pf s, (a, s'') <- apply pv s' ]


instance Applicative Prs where
--  pure :: a -> Prs a
    pure a = Prs fun where
        fun str = Just (a, str)
--  (<*>) :: Prs (a -> b) -> Prs a -> Prs b
    (<*>) prsFun prsVal = Prs fun where
        fun str = do
            (g, str')  <- runPrs prsFun str
            (a, str'') <- runPrs prsVal str'
            return (g a, str'')


satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser fun where
    fun ""                   = []
    fun (c:cs) | predicate c = [(c,cs)]
               | otherwise   = []

lower :: Parser Char
lower = satisfy isLower

char :: Char -> Parser Char
char c = satisfy (== c)

digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

multiplication :: Parser Int
multiplication = (*) <$> digit <* char '*' <*> digit




newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE fun where
    fun ""                   = Left "unexpected end of input" 
    fun (c:cs) | predicate c = Right (c,cs)
               | otherwise   = Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
--  fmap :: (a -> b) -> PrsE a -> PrsE b
    fmap f parser = PrsE fun where
        fun str = do
            (a, str') <- runPrsE parser str
            return (f a, str')

instance Applicative PrsE where
--  pure :: a -> PrsE a
    pure a = PrsE fun where
        fun str = Right (a, str)
--  (<*>) :: PrsE (a -> b) -> PrsE a -> PrsE b
    (<*>) prsFun prsVal = PrsE fun where
        fun str = do
            (g, str')  <- runPrsE prsFun str
            (a, str'') <- runPrsE prsVal str'
            return (g a, str'')


{-

class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    Nothing 'mappend' m = m
    m 'mappend' Nothing = m
    Just m1 'mappend' Just m2 = Just (m1 'mappend' m2)


instance Monoid (Maybe a) where
    mempty = Nothing
    Nothing 'mappend' m = m
    m       'mappend' _ = m

newtype First a = First { getFirst :: Maybe a }

-}





{-

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

infixl 3 <|>

instance Alternative [] where
    empty = []
    (<|>) = (++)


instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> r = l


Законы для класса типов Alternative
Помимо законов моноидальной структуры требуют выполнения

I. Right distributivity of <*>
(f <|> g) <*> a = (f <*> a) <|> (g <*> a)

II. Right absorption for <*>
empty <*> a = empty

III. Left ditributivity of fmap
f <$> (a <|> b) = (f <$> a) <|> (f <$> b)

IV. Left absorption for fmap
f <$> empty = empty


-}

instance Alternative Parser where
--  empty :: Parser a
    empty = Parser f where
        f _ = []

--  (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p q = Parser f where
        f s = let ps = apply p s
            in if null ps
            then apply q s
            else ps






instance Alternative Prs where
--  empty :: Prs a
    empty = Prs fun where
        fun str = Nothing
    
--  (<|>) :: Prs a -> Prs a -> Prs a
    (<|>) firstPrs secondPrs = Prs fun where
        fun str = (runPrs firstPrs str) <|> (runPrs secondPrs str)



lowers :: Parser String
lowers = pure (:) <*> lower <*> lowers <|> pure ""


many :: Parser a -> Parser [a]
many p = pure (:) <*> p <*> many p <|> pure []

many1 :: Prs a -> Prs [a]
many1 p = pure (:) <*> p <*> (many1 p <|> pure [])


many1Prs :: Prs a -> Prs [a]
many1Prs p = pure (:) <*> p <*> (many1Prs p <|> pure [])



mult :: Prs Int
mult = (*) <$> nat <* charPrs '*' <*> nat

nat :: Prs Int
nat = read <$> (many1Prs $ satisfyPrs isDigit)