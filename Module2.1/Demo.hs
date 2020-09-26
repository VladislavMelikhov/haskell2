module Demo where

import Prelude hiding(foldl, foldr, null)

import Data.Foldable 
import Data.Monoid

{-

foldr :: (a -> b -> b) -> b -> [a] -> b
foldl :: (b -> a -> b) -> b -> [a] -> b

foldr f ini [x1, x2, x3] =  x1 `f` (x2 `f` (x3 `f` ini))
foldl f ini [x1, x2, x3] = ((ini `f` x1) `f` x2) `f` x3

-}



{-

class Foldable t where
    foldr :: (a -> b -> b) -> b -> [a] -> b
    foldl :: (b -> a -> b) -> b -> [a] -> b
    ...


instance Foldable [] where
    foldr _ ini []     = ini
    foldr f ini (x:xs) = f x (foldr f ini xs)

    foldl _ ini []     = ini
    foldl f ini (x:xs) = foldl f (f ini x) xs


instance Foldable Maybe where
    foldr _ ini Nothing  = ini
    foldr f ini (Just x) = f x ini

    foldr _ ini Nothing  = ini
    foldr f ini (Just x) = f ini x


instance Foldable (,) c where
  --foldr :: (a -> b -> b) -> b -> (,) c a -> b
    foldr f ini (_,y) = f y ini
-} 

data Triple a = Tr a a a deriving(Eq, Show)

instance Foldable Triple where
    foldr f ini (Tr x1 x2 x3) =  x1 `f` (x2 `f` (x3 `f` ini))
    foldl f ini (Tr x1 x2 x3) =  ((ini `f` x1) `f` x2) `f` x3




data Tree a = Nil | Branch (Tree a) a (Tree a) deriving(Eq, Show)

testTree = Branch (Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)) 4 (Branch Nil 5 Nil)
{-
     4
    / \
   2   5
  / \
 1   3

-}

instance Foldable Tree where
    foldr f ini Nil            = ini
--  PreOrder
--  foldr f ini (Branch l x r) = f x  (foldr f (foldr f ini r) l)
--  InOrder
    foldr f ini (Branch l x r) = foldr f (f x (foldr f ini r)) l

newtype Preorder a   = PreO   (Tree a)    deriving (Eq, Show)
newtype Postorder a  = PostO  (Tree a)    deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a)    deriving (Eq, Show)

instance Foldable Preorder where
    foldr f ini (PreO Nil)            = ini
    foldr f ini (PreO (Branch l x r)) = f x (foldr f (foldr f ini (PreO r)) (PreO l))


instance Foldable Postorder where
    foldr f ini (PostO Nil)            = ini
    foldr f ini (PostO (Branch l x r)) = foldr f (foldr f (f x ini) (PostO r)) (PostO l)


instance Foldable Levelorder where
    foldr f ini (LevelO tree) = foldr f ini (toList [] [tree]) where
        toList :: [a] -> [Tree a] -> [a]
        toList ini []                     = ini
        toList ini (Nil:trees)            = toList ini trees
        toList ini ((Branch l x r):trees) = x : (toList ini (trees ++ [l,r]))
{-

origin:

instance Foldable Levelorder where    
    foldr f ini (LevelO tree) = levelorder [tree] where
        levelorder [] = ini
        levelorder (Nil:xs) = levelorder xs
        levelorder ((Branch l x r):xs) = f x (levelorder (xs ++ [l,r]))
-}

treeToList :: Tree a -> [a]
treeToList = foldr (:) []



tree1 = Branch (Branch Nil 1 (Branch Nil 2 Nil)) 3 (Branch Nil 4 Nil)
tree2 = Branch Nil 1 (Branch Nil 2 Nil)
{-
     3
    / \
   1   4
    \
     2
-}




{-
class Foldable t where
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b

    fold :: Monoid m => t m -> m
    fold = foldr mappend mempty --mconcat

    foldMap :: Monoid m => (a -> m) -> t a -> m
    --foldMap f cont = fold (fmap f cont) -- t may not be an instance of Functor
    foldMap f cont = foldr (mappend . f) mempty cont 


    sum :: Num a => t a -> a
    sum = getSum . foldMap Sum

    product :: Num a => t a -> a
    product = getProduct . foldMap Product

    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

    toList :: t a -> [a]
    toList = foldr (:) []

    length :: t a -> Int

    maximum :: Ord a => t a -> a

    minimum :: Ord a => t a -> a

    elem :: Eq a => a -> t a -> Bool
    


    foldr' :: (a -> b -> b) -> b -> t a -> b

    foldl' :: (b -> a -> b) -> b -> t a -> b

    foldr1 :: (a -> a -> a) -> t a -> a

    foldl1 :: (a -> a -> a) -> t a -> a



    and :: Foldable t => t Bool -> Bool

    or :: Foldable t => t Bool -> Bool

    any :: Foldable t => (a -> Bool) -> t a -> Bool

    all :: Foldable t => (a -> Bool) -> t a -> Bool

    

    concat :: Foldable t => t [a] -> [a]
    concat = fold -- На самом деле не так

    concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
    concatMap = foldMap


    newtype Endo a = Endo { appEndo :: a -> a }

    instance Monoid (Endo a) where
        mempty = Endo id
        Endo f `mappend` Endo g = Endo (f . g)
    
-}

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

{-
foldr f ini [x1,x2,x3]
    = f x1 (f x2 (f x3 ini))
    = f x1 ((f x2 . f x3) ini)
    = (f x1 . f x2 . f x3) ini -- все аппликации f xi :: b -> b, то есть являются эндоморфизмами
    = appEndo (Endo (f x1) <> Endo (f x2) <> Endo (f x3)) ini
    = appEndo ((Endo . f) x1 <> (Endo . f) x2 <> (Endo . f) x3) ini
    = appEndo ((Endo . f) x1 <> (Endo . f) x2 <> (Endo . f) x3 <> mempty) ini
    = appEndo (foldMap (Endo . f) [x1,x2,x3]) ini


foldr :: (a -> b -> b) -> b -> t a -> b
foldr f ini cont = appEndo (foldMap (Endo . f) cont) ini
-}



{-
newtype Dual a = Dual { getDual :: a }


instance Monoid a => Monoid (Dual a) where
    mempty = Dual mempty
    Dual x `mappend` Dual y = Dual (y `mappend` x)


foldl :: (b -> a -> b) -> b -> t a -> b
foldl f ini cont = appEndo (getDual (foldMap (Dual . Endo . flip f) cont)) ini

-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

instance (Foldable f, Foldable g) => Foldable ((|.|) f g) where
  --foldr :: (a -> b -> b) -> b -> |.| f g a -> b
    foldr fun ini (Cmps cont) = foldr (\g b -> foldr fun b g) ini cont

  --foldMap :: Monoid m => (a -> m) -> |.| f g a -> m
    foldMap fun (Cmps cont) = foldMap (foldMap fun) cont