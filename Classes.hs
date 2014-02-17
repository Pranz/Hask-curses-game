
{-# LANGUAGE  GADTs                     #-}
{-# LANGUAGE  MultiParamTypeClasses     #-}
{-# LANGUAGE  FunctionalDependencies    #-}
{-# LANGUAGE  ConstraintKinds           #-}
{-# LANGUAGE  RankNTypes                #-}
{-# LANGUAGE  FlexibleInstances         #-}
{-# LANGUAGE  ExistentialQuantification #-}

module Classes where 
  
import UI.NCurses (ColorID, Curses)
import Control.Monad.Trans.State
import Control.Lens
import Control.Applicative
import Data.Function (on)

import Util

data Is  c     = forall a. (c a)        => Is  a
data Is' c1 c2 = forall a. (c1 a, c2 a) => Is' a

conmap :: forall b c.          (forall a.  c a               => a -> b) -> Is c -> b
conmap f (Is x) = f x

conmap' :: forall b c1 c2.     (forall a. (c1 a, c2 a)       => a -> b) -> Is' c1 c2 -> b
conmap' f (Is' x) = f x

conmap'' :: forall b c1 c2 c3. (forall a. (c1 a, c2 a, c3 a) => a -> b) -> Is'' c1 c2 c3 -> b
conmap'' f (Is' x) = f x

class Representable e where
    char  :: Getter e Char
    color :: Getter e (Curses ColorID)
    
class Agent e w | e -> w where
    actWith:: e -> StateT w Curses ()

class Static e where
    blocks :: e -> Bool

