{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ConstraintKinds           #-}

module Curserts.Level.Room where

import Data.Map as M
import Curserts.Classes.Classes
import Control.Lens (makeLenses)

import Curserts.Data
import Curserts.Geometry

data Room = Room
	{ _roomObjects  :: Rectangle -> M.Map Vector (Is MapObject)
	, _roomEntities :: [Entity]
	}
makeLenses ''Room
data Level = Level
	{ _rooms       :: [Room]
	, _levelNumber :: Int
	}
makeLenses ''Level

simpleRoom :: Room
simpleRoom =  Room (const M.empty) []