{-# LANGUAGE TypeOperators #-}
module Demo where

import Control.Applicative


{-
To allow using Type Opertors:

:set -XTypeOperators
-}

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }
    deriving (Eq,Show)



type A   = ((,) Integer |.| (,) Char) Bool
type B t = ((,,) Bool (t -> t) |.| Either String) Int
type C   = (|.|) ((->) Bool) ((->) Integer) Integer

a :: A
a = Cmps (42, ('a', True))

b :: B t
b = Cmps (True, id, Right 42)

c :: C
c  = Cmps (const (const 42))




instance (Functor f, Functor g) => Functor (f |.| g) where
--  fmap :: (a -> b) -> (|.|) f g a -> (|.|) f g b
    fmap h (Cmps x) = Cmps $ fmap (fmap h) x

{-
x :: f (g a)
y :: f (g b)

phi :: g a -> g b

fmap :: (a -> b) -> g a -> g b
h :: a -> b
fmap h :: g a -> g b


fmap phi x = y
-}

newtype Cmps3 f g h a = Cmps3 { getCmps3 :: f (g (h a)) } 
  deriving (Eq,Show) 

instance (Functor f, Functor g, Functor h) => Functor (Cmps3 f g h) where
  fmap fun (Cmps3 a) = Cmps3 $ fmap (fmap (fmap fun)) a



{-

(1) fmap id cont == cont
(1) fmap id cont == id cont
(1) fmap id      == id

fmap id (Cmps x)            --def fmap (Cmps)
== Cmps $ fmap (fmap id) x  --(1) fmap (g)
== Cmps $ fmap id x         --(2) fmap (f)
== Cmps x
-}



{-

Определение fmap: 
fmap h (Cmps x) = Cmps $ fmap (fmap h) x

Второй закон для функторов:
(2) fmap f (fmap g cont) == fmap (f . g) cont

Вспомним определение композиции
(a . b) x = a (b x) 

В левой части (2) видим не что иное, как композицию функций (fmap f) и (fmap g)
т.е. 
fmap f (fmap g cont) == ((fmap f) . (fmap g)) cont

Будет удобно записать (2) в следующем виде
(2')  ((fmap f) . (fmap g)) cont  == fmap (f . g) cont
(2'')  (fmap f) . (fmap g)        == fmap (f . g)


Нужно доказать, что:
fmap h2 (fmap h1 (Cmps x)) = fmap (h2 . h1) (Cmps x)

fmap h2 (fmap h1 (Cmps x))                   -- def fmap (Cmps)
== fmap h2 (Cmps $ fmap (fmap h1) x)         -- def fmap (Cmps)

== Cmps $ fmap (fmap h2) (fmap (fmap h1) x)  -- (2), здесь f = (fmap h2), g = (fmap h1)
                                                
== Cmps $ fmap ((fmap h2) . (fmap h1)) x     -- (2'')    
== Cmps $ fmap (fmap (h2 . h1)) x            -- def fmap (Cmps)
== fmap (h2 . h1) (Cmps x)

-}


instance (Applicative f, Applicative g) => Applicative (f |.| g) where
--pure :: a -> |.| f g a
  pure = Cmps . pure . pure

--(<*>) :: g (a -> b) -> (g a -> g b)

--fun :: f (g (a -> b))
--phi :: f (g a -> g b)

--phi = fmap (<*>) fun

--a :: f (g a)

--(<*>) :: |.| f g (a -> b) -> |.| f g a -> |.| f g b
  (<*>) (Cmps fun) (Cmps a) = Cmps $ (fmap (<*>) fun) <*> a 



unCmps3 :: Functor f => (f |.| g |.| h) a -> f (g (h a))
unCmps3 = (fmap getCmps) . getCmps

unCmps4 :: (Functor f2, Functor f1) => (f2 |.| f1 |.| g |.| h) a -> f2 (f1 (g (h a)))
unCmps4 = getCmps . (fmap getCmps) . Cmps . (fmap getCmps) . getCmps