module Demo where

import Prelude hiding(foldr, foldl)
import Data.Foldable
import Control.Applicative
import Data.Traversable

data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = f x (foldr f (foldr f ini r) l) --preorder [4,2,1,3,5]

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
{-
        4
       / \
      2   5
     / \
    1   3
-}



{-

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty
-}


{-
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())
-- (*>) :: f a -> f () -> f ()

sequenceA_ [ap1,ap2,ap3] = ap1 *> (ap2 *> (ap3 *> pure ()))

-}

{-
sequenceA_ $ fmap print testTree

    2
   / \
  1   4
     / \
    3   5

preorder: 21435
-}


{-
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldr mappend mempty

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap fun = foldr (mappend . fun) mempty

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ fun = foldr ((*>) . fun) (pure ())
-}



sequenceA2List :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2List = foldr (\x y -> pure (:) <*> x <*> y) (pure [])

tree = Branch (Branch Nil ("A",1) Nil) ("B",2) (Branch Nil ("C",3) Nil)

{-
sequenceA2 :: (Foldable t, Applicative f) => t (f a) -> f (t a)
Невозможно перевозродить стркутуру t - контекста Foldable недостаточно для sequenceA

-}

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list fun = foldr (\x y -> pure (:) <*> (fun x) <*> y) (pure [])


{-
class (Functor t, Foldable t) => Traversable t where
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id

    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse fun = traverse . fmap fun
-}



{-
instance Traversable Maybe where
  --traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    traverse _ Nothing  = pure Nothing
    traverse g (Just x) = pure Just <*> g x

Сравнение с функтором
instance Functor Maybe where
  --fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap g (Just x) = Just (g x)
-}



{-
instance Functor (,) s where
  --fmap :: (a -> b) -> (s, a) -> (s, b)
    fmap f (x,y) = (,) x (f y)

instance Traversable (,) s where
  --traverse :: Applicative f => (a -> f b) -> (s, a) -> f (s, b)
    traverse f (x,y) = ((,) x) <$> (f y)
-}

data Triple a = Tr a a a
    deriving (Eq,Show)

instance Functor Triple where
  --fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Tr x1 x2 x3) = Tr (f x1) (f x2) (f x3)

instance Foldable Triple where
  --foldr :: (a -> b -> b) -> b -> Triple a -> b
    foldr f ini (Tr x1 x2 x3) = foldr f ini [x1,x2,x3]

instance Traversable Triple where
  --traverse :: Applicative f => (a -> f b) -> Triple a -> f (Triple b)
    traverse f (Tr x1 x2 x3) = Tr <$> f x1 <*> f x2 <*> f x3

instance Applicative Triple where
  --pure :: a -> Triple a
    pure x = Tr x x x
  --(<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (<*>) (Tr f1 f2 f3) (Tr x1 x2 x3) = (Tr (f1 x1) (f2 x2) (f3 x3))



data Result a = Ok a | Error String
    deriving (Eq,Show)

instance Functor Result where
  --fmap :: (a -> b) -> Result a -> Result b
    fmap f (Ok x)      = Ok (f x)
    fmap _ (Error str) = Error str

instance Foldable Result where
  --foldr :: (a -> b -> b) -> b -> Result a -> b
    foldr f ini (Ok x)      = f x ini
    foldr _ ini (Error str) = ini

instance Traversable Result where
  --traverse :: (a -> f b) -> Result a -> f (Result b)
    traverse f (Ok x)      = Ok <$> (f x)
    traverse _ (Error str) = pure (Error str)



{-
instance Functor [] where
  --fmap :: (a -> b) -> [a] -> [b]
    fmap _ []     = []
    fmap f (x:xs) = (:) (f x) (fmap f xs)

instance Traversable [] where
  --traverse :: (a -> f b) -> [a] -> f [b]
    traverse _ []     = pure []
    traverse f (x:xs) = (:) <$> f x <*> fmap f xs
-}



{-
traverse (\x -> [x+10,x+20]) [1,2,3] ==
(:) <$> [11,21] <*> ((:) [12,22] <*> ((:) <$> [13,23] <*> pure []))
-}


{-
data Tree a = Nil | Branch (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
  --fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Nil            = Nil
    fmap f (Branch l x r) = Branch (fmap f l) (f x) (fmap f r)

instance Foldable Tree where
  --foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ ini Nil = ini
    foldr f ini (Branch l x r) = f x (foldr f (foldr f ini r) l)
-}


instance Traversable Tree where
  --traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse _ Nil            = pure Nil
    traverse f (Branch l x r) = Branch <$> traverse f l <*> f x <*> traverse f r


infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }
    deriving (Eq,Show)

instance (Functor f, Functor g) => Functor ((|.|) f g) where
  --fmap :: (a -> b) -> |.| f g a -> |.| f g b
    fmap fun (Cmps f) = Cmps $ fmap (fmap fun) f 

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  --foldr :: (a -> b -> b) -> b -> |.| f g a -> b
    foldr fun ini (Cmps f) = foldr (\g x -> foldr fun x g) ini f

instance (Traversable f, Traversable g) => Traversable ((|.|) f g) where
  --traverse :: (a -> t b) -> |.| f g a -> t (|.| f g b)
    traverse fun (Cmps f) = Cmps <$> traverse (traverse fun) f
    
    -- fun                     :: (a       -> t b        )
    -- traverse fun            :: (g a     -> t (g b)    )
    -- traverse (traverse fun) :: (f (g a) -> t (f (g b)))