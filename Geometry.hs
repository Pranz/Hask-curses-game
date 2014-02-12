{-# Language TemplateHaskell #-}

module Geometry
  ( Vector(Vector)
  , x
  , y
  , Locatable
  , location
  , getLocation
  , setLocation
  , negateVector
  , hypotenuse
  , distanceFromPoints
  , overCoords
  ) where

import Data.Monoid
import Control.Lens
import Control.Applicative
import Data.Function (on)
import Util

data Vector = Vector {_x ::Int, _y ::Int}
    deriving (Show, Eq, Read)
makeLenses ''Vector

class Locatable a where
    location    ::Lens' a Vector
    location    = lens getLocation (flip setLocation)
    getLocation ::a -> Vector
    getLocation = (^.location)
    setLocation ::Vector ->a ->a
    setLocation = set location

instance Monoid Vector where
    mempty  = Vector 0 0
    mappend (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

instance Locatable Vector where
    getLocation = id
    setLocation = const

hypotenuse ::(Floating b, Locatable a) => a -> b
hypotenuse = liftA2 ((sqrt . fromIntegral .: (+)) `on` (^2)) (^.location.x) (^.location.y)

negateVector ::Vector -> Vector
negateVector (Vector x y) = Vector (-x) (-y)

distanceFromPoints ::Floating b => Vector -> Vector -> b
distanceFromPoints = hypotenuse .: (mappend . negateVector)

overCoords :: (Int -> Int -> a) -> Vector -> a
overCoords f = liftA2 f (view x) (view y)