module Demo where

import Data.Functor
import Data.Function
import Control.Monad
import Control.Applicative

{-

($)   ::                    (a -> b) ->   a ->   b
(<$>) :: Functor f     =>   (a -> b) -> f a -> f b
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
(=<<) :: Monad m       => (a -> m b) -> m a -> m b

(&)    ::                    a ->   (a -> b) ->   b -- Data.Function
(<&>)  :: Fuctor f      => f a ->   (a -> b) -> f b -- Control.lens.Operators
(<**>) :: Applicative f => f a -> f (a -> b) -> f b -- Control.Applicative
(>>=)  :: Monad m       => m a -> (a -> m b) -> m b
-}

infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

(<***>) :: Applicative f => f a -> f (a -> b) -> f b
(<***>) = flip (<*>)

{-

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
xs <**> fs = pure (&) <*> xs <*> fs

-}

{-
funM :: (Ord a, Num a, Monad m) => m a -> m a
funM mv = do 
    v <- mv
    if v > 0 then return (v^2) else fail ""

funA :: (Num a, Applicative f) => f a -> f a
funA mv = pure (^2) <*> mv
-}


{-
Old definition of Monad (before 7.10)

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    m >> n = m >>= \_ -> n
    fail   :: String -> m a


New definition of Monad (since 7.10)

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    -- (>>) = (*>)
    m >> n = m >>= \_ -> n
    fail   :: String -> m a
-}

{-
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

ap :: Monad m => m (a -> b) -> m a -> m b
ap fs xs = do { f <- fs; x <- xs; return (f x) }
ap fs xs = fs >>= (\f -> xs >>= (\x -> return (f x)))
ap fs xs = fs >>= \f -> xs >>= \x -> return (f x)
ap fs xs = fs >>= \f -> xs >>= return . f


fmap :: Functor f => (a -> b) -> f a -> f b

liftM :: Monad m => (a1 -> r) -> m a1 -> m r
liftM f xs = do { x <- xs; return (f x) }

liftM f xs = xs >>= \x -> return (f x)

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f xs ys = do { x <- xs; y <- ys; return (f x y) }
-}

{-
Законы для монад

I. Left identity:
return a >>= f == f a

II. Right identity:
m >>= return == m

III. Associativity:
(m >>= f) >>= g == m >>= (\x -> f x >>= g)
-}

{-
Законы для функторов:
I.  fmap id == id
I'. fmap id xs == xs

fmap id xs
== liftM id xs                    --def liftM
== xs >>= \x -> return (id x)     --def id
== xs >>= \x -> return x          --eta rule
== xs >>= return                  --Right identity (monad)
== xs

II.  fmap (f . g) == (fmap f) . (fmap g)
II'. fmap (f . g) xs == fmap f (fmap g xs)

fmap f (fmap g xs)
== liftM f (liftM g xs)                                     --def liftM
== (xs >>= (\x -> return (g x))) >>= (\y -> return (f y))   --Associativity (monad)
== xs >>= (\x -> return (g x) >>= (\y -> return (f y))      --Left identity (monad)
== xs >>= (\x -> (\y -> return (f y)) (g x))
== xs >>= (\x -> return (f (g x)))
== xs >>= (\x -> return ((f . g) x))                        --def liftM
== liftM (f . g) xs
== fmap (f . g) xs


-}

newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE predicate = PrsE fun where
    fun ""                   = Left "unexpected end of input" 
    fun (c:cs) | predicate c = Right (c,cs)
               | otherwise   = Left ("unexpected " ++ [c])

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Monad PrsE where
  --return :: a -> PrsE a
    return x = PrsE (\str -> Right (x, str))

  --(>>=) :: PrsE a -> (a -> PrsE b) -> PrsE b
    (>>=) parser fun = PrsE parse where
        parse string = do
            (value,tail) <- runPrsE parser string
            runPrsE (fun value) tail


data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Bi x1 y1 c1) c2 c3      = Bi x1 y1 (concat3OC c1 c2 c3)
concat3OC (Un x1) (Bi x2 y2 c2) c3 = Bi x1 x2 (concat3OC (Un y2) c2 c3)
concat3OC (Un x1) (Un x2) c3       = Bi x1 x2 c3


concatOC :: OddC (OddC a) -> OddC a
concatOC (Un c)     = c
concatOC (Bi x y c) = concat3OC x y (concatOC c)


tst1' = Bi 'a' 'b' (Un 'c')
tst2' = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3' = Bi 'i' 'j' (Un 'k')


instance Functor OddC where
  --fmap :: (a -> b) -> OddC a -> OddC b
    fmap f (Un x)     = Un (f x)
    fmap f (Bi x y c) = Bi (f x) (f y) (fmap f c)

instance Applicative OddC where
  --pure :: a -> OddC a
    pure = return
  --(<*>) :: OddC (a -> b) -> OddC a -> OddC b
    (<*>) fs args = do { f <- fs; arg <- args; return (f arg) }

instance Monad OddC where
  --return :: a -> OddC a
    return = Un
  --(>>=) :: OddC a -> (a -> OddC b) -> OddC b
    (>>=) args fs = concatOC $ fmap fs args


tst1 = Bi 10 20 (Un 30)
tst2 = Bi 1 2 (Bi 3 4 (Un 5))


{-
Трансляция

do pat <- computation   >>>     let f pat = more
   more                 >>>         f _   = fail "..."
                        >>>     in computation >>= f




MonadFail Proposal (начала реализовываться в GHC 8.0)

class Monad m => MonadFail m where
    fail :: String -> m a

> f xs = do { True <- xs; return () }
> :t f
f :: MonadFail m => m Bool -> m ()

> f' xs = do { ~True <- xs; return () }
> :t f'
f' :: Monad m => m Bool -> m ()
-}