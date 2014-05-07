{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ConstraintKinds           #-}

module Curserts.Level.Room where

import Data.Map as M
import Curserts.Types.Classes
import Control.Lens (makeLenses)

import Curserts.Data
import Curserts.Geometry

minRoomWidth  = 7  :: Int
maxRoomWidth  = 10 :: Int
minRoomHeight = 7  :: Int
maxRoomHeight = 10 :: Int

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
