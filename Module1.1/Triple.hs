module Triple where

import Data.Functor((<$>))
import Control.Applicative

data Triple a = Tr a a a deriving(Eq, Show)

instance Functor Triple where
  --fmap :: (a -> b) -> Triple a -> Triple b
    fmap g (Tr x1 x2 x3) = Tr (g x1) (g x2) (g x3)

instance Applicative Triple where
  --pure :: a -> Triple a
    pure x = Tr x x x
  --(<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (<*>) (Tr g1 g2 g3) (Tr x1 x2 x3) = Tr (g1 x1) (g2 x2) (g3 x3)