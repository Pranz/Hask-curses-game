{-# LANGUAGE TemplateHaskell #-}

module Game 
  ( loop
  , initWorld
  , World
  , initGame
  ) where

import UI.NCurses
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Monoid
import Control.Lens
import Control.Monad
import Data.Function (on)
import qualified Data.Map as M

import Classes
import Geometry

data Entity = Entity
  { _entPosition ::Vector
  , _entChar ::Char
  , _entColor ::Curses ColorID
  }
makeLenses ''Entity
data World = World
  { _map  ::M.Map Int (M.Map Int MapObject)
  , _hero ::Entity
  }
makeLenses ''World

instance Locatable Entity where
    location = entPosition

instance Representable Entity where
    char  = entChar
    color = entColor

instance Static Entity where
    blocks = const True

ent       = Entity (Vector 5 3) '@' undefined
initWorld = World M.empty ent

initGame :: StateT World Curses ()
initGame = do
    lift $ setEcho False
    w <-lift defaultWindow
    loop w

loop ::Window -> StateT World Curses ()
loop w = do
    heroLoc  <- use $ hero.location
    heroChar <- use $ hero.char
    lift $ (updateWindow w $ do
        (flip moveCursor `on` fromIntegral) `overCoords` heroLoc
        drawString.return $ heroChar
        moveCursor 0 0)
    hero.location.x += 1
    lift render
    ev <- lift $ (getEvent w Nothing)
    if (mfilter (== EventCharacter 'q') ev) ==Nothing then loop w else return ()