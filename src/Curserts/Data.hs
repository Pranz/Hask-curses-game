{-# LANGUAGE TemplateHaskell           #-}

module Curserts.Data where

import Control.Lens (makeLenses)
import Data.Map as M
import UI.NCurses
import System.Random (StdGen)

import Curserts.Types.Classes
import Curserts.Geometry
import Curserts.Util (constLens)

data Wall = Wall deriving (Show, Read, Eq)
data Entity = Entity
  { _entPosition :: Vector
  , _entChar     :: Char
  , _entColor    :: ColorID
  }
makeLenses ''Entity
data World = World
  { _levelmap   :: M.Map Vector (Is MapObject)
  , _hero       :: Entity
  , _testString :: String
  , _gamelog    :: [String]
  , _turnNumber :: Int
  , _rooms      :: [(Vector, Vector)]
  , _randomgen  :: StdGen
  }
makeLenses ''World

instance Locatable Entity where
    location = entPosition

instance Representable Entity where
    char  = entChar
    color = entColor

instance Static Entity where
    blocks = constLens True

instance Representable Wall where
    char  = constLens '#'
    color = constLens defaultColorID

instance Static Wall where
    blocks = constLens True
    
instance MapObject Wall
