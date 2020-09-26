module Demo where

import Control.Applicative hiding (ZipList, getZipList)

{-

instance Applicative [] where
    pure x = [x]
    gs <*> xs = [ g x | g <- gs, x <- xs ]

-}


foo :: [] String
foo = do 
        x <- ["1", "2"] 
        y <- ["a", "b"] 
        return (x ++ y)

bar :: Maybe Int
bar = do 
    x <- (Just 3)
    y <- (Just 5)
    return (x + y) 


newtype ZipList a = ZipList { getZipList :: [a] }
    deriving Show

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)


x1s = [1,2,3]
x2s = [4,5,6]
x3s = [7,8,9]
x4s = [10,11,12]

infixl 4 >$<

(>$<) :: (a -> b) -> [] a -> [] b
(>$<) g xs = getZipList (g <$> ZipList xs)

infixl 4 >*<

(>*<) :: [] (a -> b) -> [] a -> [] b
(>*<) gs xs = getZipList (ZipList gs <*> ZipList xs)



{-

instance Applicative (Either e) where
    pure          = Right
    Left  e <*> _ = Left e
    Right g <*> r = fmap g r



instance Monoid e => Applicative ((,) e) where
    pure x = (mempty, x)
    (u, g) <*> (v, x) = (u 'mappend' v, g x)

-}

divideList :: Fractional a => [a] -> a
divideList []     = 1
divideList (x:xs) = (/) x (divideList xs)

divideList' :: (Show a, Fractional a) => [a] -> (String, a)
divideList' []     = ("1.0", 1)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++"/", x) <*> divideList' xs


{-

instance Applicative ((->) e) where
    pure x = \e -> x
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
    (<*>) g h = \e -> g e (h e)

zip <*> tail $ [1..] == [(1, 2), (2, 3), ... ]

pascalTriangle = iterate nextRow [1]
    where nextRow xs = (zipWith (+)) <*> tail $ [0] ++ xs ++ [0]
-}

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }


instance Functor (Arr2 e1 e2) where
  --fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
    fmap g (Arr2 arr2) = Arr2 (\x y -> g (arr2 x y))


instance Functor (Arr3 e1 e2 e3) where
  --fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
    fmap g (Arr3 arr3) = Arr3 (\x y z -> g (arr3 x y z))


instance Applicative (Arr2 e1 e2) where
  --pure :: a -> Arr2 e1 e2 a
    pure x = Arr2 (\e1 e2 -> x)
  --(<*>) :: Arr2 e1 e2 (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
    (<*>) (Arr2 g) (Arr2 h) = Arr2 (\e1 e2 -> g e1 e2 (h e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  --pure :: a -> Arr2 e1 e2 e3 a
    pure x = Arr3 (\e1 e2 e3 -> x)
  --(<*>) :: Arr2 e1 e2 e3 (a -> b) -> Arr2 e1 e2 e3 a -> Arr2 e1 e2 e3 b
    (<*>) (Arr3 g) (Arr3 h) = Arr3 (\e1 e2 e3 -> g e1 e2 e3 (h e1 e2 e3))

{-

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (<*) :: f a -> f b -> f a
    u <* v = undefined
    u <* v = pure const <*> u <*> v
    (*>) :: f a -> f b -> f b
    u *> v = undefined
    u *> v = pure (flip const) <*> u <*> v



liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b :: pure f <*> a <*> b

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c :: pure f <*> a <*> b <*> c


(<**>) :: Applicative f => f a -> f (a -> b) -> f b

--Version I
(<**>) x g = liftA2 (flip ($)) x g
    
    x :: f a
    g :: f (a -> b)

    (flip ($)) :: (a -> (a -> b) -> b) 
    ($) :: (a -> b) -> a -> b

--Version II
(<**>) = flip (<*>)

-}