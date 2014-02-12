{-# Language ExistentialQuantification #-}
{-# Language MultiParamTypeClasses     #-}
{-# Language FunctionalDependencies    #-}

module Classes 
  ( Representable
  , Static
  , Agent
  , MapObject
  , char
  , color
  , act
  , blocks
  , getMapObject
  ) where

import UI.NCurses (ColorID, Curses)
import Control.Monad.Trans.State
import Control.Lens

class Representable e where
    char  ::Lens' e Char
    color ::Lens' e (Curses ColorID)
    
class Agent e w | e -> w where
    actWith::e -> StateT w Curses ()

class (Representable e) => Static e where
    blocks :: e -> Bool

data MapObject = forall a . Static a => MkMapObject {getMapObject :: a}