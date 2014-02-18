{-# Language TemplateHaskell #-}

module Curserts.Geometry where

import Data.Monoid
import Control.Lens
import Control.Applicative
import Data.Function (on)
import Prelude hiding (Left, Right)

import Curserts.Util

data Vector = Vector {_x ::Int, _y ::Int}
    deriving (Show, Eq, Read, Ord)
makeLenses ''Vector

data Rectangle = Rectangle {_rectanglePosition :: Vector, _rectangleDimensions :: Vector}
    deriving (Show, Eq, Read, Ord)
makeLenses ''Rectangle

data Direction = Left | Up | Right | Down | LeftUp | RightUp | LeftDown | RightDown
    deriving (Show, Eq, Read)
    
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
    
instance Locatable Rectangle where
    location = rectanglePosition

hypotenuse ::(Floating b, Locatable a) => a -> b
hypotenuse = liftA2 ((sqrt . fromIntegral .: (+)) `on` (^2)) (^.location.x) (^.location.y)

negateVector ::Vector -> Vector
negateVector (Vector x y) = Vector (-x) (-y)

distanceFromPoints ::Floating b => Vector -> Vector -> b
distanceFromPoints = hypotenuse .: (mappend . negateVector)

overCoords :: (Int -> Int -> a) -> Vector -> a
overCoords f = liftA2 f (view x) (view y)

direction2vector :: Direction -> Vector
direction2vector dir = case dir of
    Left  -> Vector (-1) 0
    Right -> Vector 1 0
    Up    -> Vector 0 (-1)
    Down  -> Vector 0 1
    LeftUp    -> Vector (-1) (-1)
    RightUp   -> Vector 1 (-1)
    LeftDown  -> Vector (-1) 1
    RightDown -> Vector 1 1

rectangleArea :: Rectangle -> Int
rectangleArea =  overCoords (*) . _rectangleDimensions

secondRectanglePosition :: Rectangle -> Vector
secondRectanglePosition = liftA2 mappend _rectanglePosition _rectangleDimensions