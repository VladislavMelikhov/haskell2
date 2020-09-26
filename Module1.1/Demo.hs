module Demo where

import Data.Functor((<$>))
import Data.Monoid
import Prelude hiding (pure)

{-

newtype Identity a = Identity a

data Maybe a = Nothing | Just a

data (,) a = (,) a b

data Either a b = Left a | Right b

data [] a = [] | a : [] a

data Tree a = Leaf | Branch (Tree a) a (Tree a)


-}


{-

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor Identity where
    fmap g (Identity x) = Identity (g x)

instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap g (Just x) = Just (g x)

instance Functor [] where
    fmap = map

instance Functor (Either e) where
  --fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left x)  = (Left x)
    fmap g (Right y) = Right (g y)


instance Functor ((,) s) where
  --fmap :: (a -> b) -> (s, a) -> (s, b)
    fmap g (x,y) = (x, g y)

instance Functor ((->) e) where
  --fmap :: (a -> b) -> (e -> a) -> (e -> b)
    fmap = (.)  

-}

newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }


instance Functor (Arr2 e1 e2) where
  --fmap :: (a -> b) -> Arr2 e1 e2 a -> Arr2 e1 e2 b
    fmap g (Arr2 arr2) = Arr2 (\x y -> g (arr2 x y))


instance Functor (Arr3 e1 e2 e3) where
  --fmap :: (a -> b) -> Arr3 e1 e2 e3 a -> Arr3 e1 e2 e3 b
    fmap g (Arr3 arr3) = Arr3 (\x y z -> g (arr3 x y z))



{-

(1) fmap id cont == cont

(2) fmap f (fmap g cont) == fmap (f . g) cont



-}

{-
instance Functor (Either e) where
  --fmap :: (a -> b) -> Either e a -> Either e b
    fmap _ (Left x)  = (Left x)
    fmap g (Right y) = Right (g y)

(1)
fmap id (Left x)  -- def fmap
== Left x

fmap id (Right y) -- def fmap
== Right (id y)   -- def id
== Right y

(2)
fmap f (fmap g (Left x)) -- def fmap
== fmap f (Left x)       -- def fmap
== Left x

fmap (f . g) (Left x)    -- def fmap
== Left x

fmap f (fmap g (Right x)) -- def fmap
== fmap f (Right (g x))   -- def fmap
== Right (f (g x))      


fmap (f . g) (Right x)    -- def fmap
== Right ((f . g) x)      -- def (.)
== Right (f (g x))

-}



{-

instance Functor ((->) e) where
  --fmap :: (a -> b) -> (e -> a) -> (e -> b)
    fmap = (.)

(1)
fmap id cont == cont

fmap id fun   -- def fmap
== (.) id fun 
== id . fun   -- id является левым нейтральным элементом для композиции
== fun

(2)
fmap f (fmap g cont) == fmap (f . g) cont

fmap f (fmap g fun) -- def fmap
== fmap f (g . fun) -- def fmap
== f . (g . fun)    -- Композиция ассоциативна
== f . g . fun

fmap (f . g) fun -- def fmap
== (f . g) . fun -- Композиция ассоциативна
== f . g . fun

-} 







{-

instance Functor [] where
    fmap = map

map :: (a -> b) -> [] a -> [] b
map _ []     = []
map g (x:xs) = g x : map g xs


(1)
fmap id cont == cont

fmap id [] -- def fmap
== []


IH: fmap id xs == xs
fmap id (x:xs) == (x:xs)

fmap id (x:xs)         -- def fmap
== (id x) : fmap id xs -- def id
== x : fmap id xs      -- IH
== x : xs

Доказать:
fmap f (fmap g xs) == fmap (f . g) xs

1) Пустой список
fmap f (fmap g []) --def fmap
== fmap f []       --def fmap
== []

2) Предположение индукции
Предположим, 
fmap f (fmap g xs) == fmap (f . g) xs

3) Шаг индукции
Нужно показать, что 
fmap f (fmap g (x:xs)) == fmap (f . g) (x:xs)

fmap f (fmap g (x:xs))          -- def fmap
== fmap f (g x : fmap g xs)     -- def fmap
== f (g x) : fmap f (fmap g xs) -- используем предположение индукции
== f (g x) : fmap (f . g) xs


fmap (f . g) (x:xs)             --def fmap
== (f . g) x : fmap (f . g) xs  --def (.)
== f (g x) : fmap (f . g) xs
-} 

class Functor f => Pointed f where
    pure :: a -> f a

instance Pointed Maybe where
    pure x = Just x

instance Pointed [] where
    pure x = [x]

instance Pointed (Either e) where
  --pure :: a -> Either e a
    pure x = Right x

instance Pointed ((->) e) where
  --pure :: a -> ((->) e a)
    pure x = \e -> x

instance Monoid s => Pointed ((,) s) where
    pure x = (mempty,x)

{-
Закон для класса типов Pointed:

fmap g (pure x) == pure (g x)


-}

{-
fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
fmap3arg :: Functor f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d


-}

--fmap:: (a -> (b -> c)) -> f a - f (b -> c)
--fmap2arg :: Functor f => (a -> b -> c) -> f a -> f b -> f c
--fmap2arg g as bs = _ (fmap g as) bs
--                  :: f (b -> c)

infixl 4 <*>

class Functor f => Apply f where
    (<*>) :: f (a -> b) -> f a -> f b

instance Apply [] where
    (g:gs) <*> (x:xs) = g x : (gs <*> xs)
    _      <*> _      = []

{-
fmap2 :: Apply f => (a -> b -> c) -> f a -> f b -> f c
fmap2 g as bs = g <$> as <*> bs

fmap3 :: Apply f => (a -> b -> c) -> f a -> f b -> f c -> f d
fmap3 g as bs cs = g <$> as <*> bs <*> cs
-}


{-
import Control.Applicative

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b


instance Applicative Maybe where
  pure = Just
  Nothing <*> _  = Nothing
  (Just g) <*> x = fmap g x



Связь между Functor и Applicative
fmap g cont == pure g <*> cont

cont :: f a
g :: a -> b



<*> - функция аппликации, App
up - функция поднятия в контейнер


Законы для класса типов Applicative

I Identity
pure id <*> v == v

II Homomorphism
pure g <*> pure x == pure (g x)

III Interchange
cont <*> pure x == pure ($ x) <*> cont

IV Composition

pure (.) <*> u <*> v <*> cont == u <*> (v <*> cont)
-}

