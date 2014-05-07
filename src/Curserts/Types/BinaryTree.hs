module BinaryTree where

import Data.Traversable as T
import Data.Foldable as F
import Data.Function (on)
import Control.Applicative

data Tree a = Empty | Leaf a | Node (Tree a) (Tree a) a
    deriving (Eq, Read, Show)

instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Leaf a)     = (Leaf . f) a
    fmap f (Node l k x) = (Node `on` fmap f) l k $ f x

instance F.Foldable Tree where
    foldr f acc Empty        = acc
    foldr f acc (Leaf a)     = f a acc
    foldr f acc (Node l k x) = F.foldr f (f x $ F.foldr f acc l) k

instance T.Traversable Tree where
    traverse f Empty        = pure Empty
    traverse f (Leaf x)     = Leaf <$> f x
    traverse f (Node l k x) = (liftA2 Node `on` T.traverse f) l k <*> f x
