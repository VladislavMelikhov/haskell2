{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Demo where

import Prelude hiding(foldr,sum)
import Data.Functor
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Traversable
import Data.Foldable
import Control.Applicative

{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b

fmap id == id
fmap id cont == cont



class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

I. Identity
Traverse без эффектов не должен менять струтуру контейнера
traverse Identity == Identity
traverse Identity cont == Identity cont



newtype Identity a = Identity { runIdentity :: a }

instance Applicative Identity where
    pure = Identity
    Identity g <*> Identity x = Identity (g x)
-}



{-
class Fuctor f where
    fmap :: (a -> b) -> f a -> f b

fmap (g1 . g2) == fmap g1 . fmap g2
fmap (g1 . g2) cont = fmap g1 (fmap g2 cont)

class (Fuctor t, Foldable t) => Traverable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)


II. Composition
traverse (Compose . fmap g2 . g1) == Compose . fmap (traverse g2) . traverse g1
traverse (Compose . fmap g2 . g1) cont == Compose (fmap (traverse g2) (traverse g1 cont))

cont :: Traversable t => t a
g1   :: Applicative f1 => a -> f1 b
g2   :: Applicative f2 => b -> f2 c

traverse g1 cont :: f1 (t b)
(traverse g2) :: t b -> f2 (t c)
fmap (traverse g2) (traverse g1 cont) :: f1 (f2 (t c))
-}


{-
III. Naturality
t . traverse g == traverse (t . g)

где
t :: (Applicative f, Applicative g) => f a -> g a
произвольный аппликативный гомоморфизм, то есть функция удовлетворяющая требованиям
t (pure x) = pure x
t (x <*> y) = t x <*> t y

-}


{-
IV. Identity
sequenceA . fmap Identity == Identity

V. Composition
sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA

VI. Naturality
t . sequenceA == sequeceA . fmap t
-}

data OddC a = Un a | Bi a a (OddC a)
    deriving (Eq,Show)

cnt1 = Un 42
cnt3 = Bi 1 2 cnt1
cnt5 = Bi 3 4 cnt3
cntInf = Bi 'A' 'B' cntInf

instance Functor OddC where
  --fmap :: (a -> b) -> OddC a -> OddC b
    fmap f (Un x)          = Un (f x)
    fmap f (Bi x1 x2 tail) = Bi (f x1) (f x2) (fmap f tail)

instance Foldable OddC where
  --foldr :: (a -> b -> b) -> b -> OddC a -> b
    foldr f ini (Un x)          = f x ini
    foldr f ini (Bi x1 x2 tail) = f x1 (f x2 (foldr f ini tail))

instance Traversable OddC where
  --traverse :: Applicative f => (a -> f b) -> OddC a -> f (OddC b)
    traverse f (Un x)          = Un <$> (f x)
    traverse f (Bi x1 x2 tail) = Bi <$> (f x1) <*> (f x2) <*> (traverse f tail)










--Фантомные типы
-- :set -XGeneralizedNewTypeDeriving 
newtype Temperature a = Temperature Double
    deriving (Num, Show, Eq, Fractional)


data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)


{-
newtype Const c a = Const { getConst :: c }
    deriving(Eq, Show)

instance Functor (Const c) where
  --fmap :: (a -> b) -> Const c a -> Const c b
    fmap _ (Const v) = Const v

instance Foldable (Const c) where
  --foldMap :: Monoid m => (a -> m) -> Const c a -> m
    foldMap _ _ = mempty

instance Monoid c => Applicative (Const c) where
  --pure :: a -> Const c a
    pure _ = Const mempty

  --(<*>) :: Const c (a -> b) -> Const c a -> Const c b
    Const f <*> Const v = Const (c `mappend` v)

instance Traversable (Const c) where
  --traverse :: Applicative f => (a -> f b) -> Const c a -> f (Const c b)
    traverse _ (Const v) = pure $ Const v
-}



data Result a = Error | Ok a
    deriving (Eq, Show)

instance Traversable Result where
  --traverse :: Applicative f => (a -> f b) -> Result a -> f (Result b)
    traverse _ Error  = pure Error
    traverse f (Ok x) = pure Ok <*> (f x)

{-
fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f cont = runIdentity (traverse (Identity . f) cont)
-}

instance Functor Result where
    fmap = fmapDefault


{-
foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f cont = getConst (traverse (Const . f) cont)
-}

--Const . f :: a -> Const m x
--traverse (Const . f) cont :: Const m (t x)

instance Foldable Result where
    foldMap = foldMapDefault

{-
instance Monoid c => Applicative (Const c) where
  --pure :: a -> Const c a
    pure = Const mempty

    (<*>) :: Const (a -> b) -> Const a -> Const b
    (<*>) (Const c1) (Const c2) = Const (c1 `mappend c2)
-}

data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
    foldMap = foldMapDefault

instance Traversable Tree where
  --sequenceA :: Applicative f => Tree (f a) -> f (Tree a)
    sequenceA Nil            = pure Nil
    sequenceA (Branch l x r) = pure (flip . Branch) <*> (sequenceA l) <*> (sequenceA r) <*> x
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  --traverse f = sequenceA . (fmap f) 
  
{-
class (Functor t, Foldable t) => Traverable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)

    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA

    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = traverse
-}  