module Curserts.Generation.RoomGeneration where

import Control.Monad.Trans.State
import Curserts.Geometry
import Data.Function (on)
import Data.Map as M
import Control.Lens
import Control.Monad
import Data.Monoid

import Curserts.Types.Classes
import Curserts.Random
import Curserts.Data
import Curserts.Util

generateRoom :: Monad m => Vector -> StateT World m ()
generateRoom v = drawBorders v >> genroom (Vector 1 1) v True
    where getDim True  (Vector x _)   = x
          getDim False (Vector _ y)   = y
          setDim True  (Vector _ y) x = Vector x y
          setDim False (Vector x _) y = Vector x y
          maxOffset   = 3
          minimumArea = 120
          drawBorders (Vector x y) = forM_ [0..x] (\x' -> levelmap %= M.insert (Vector x' y) (Is Wall) >> levelmap %=  M.insert (Vector x' 0) (Is Wall)) >> forM_ [0..y] (\y' -> levelmap %= M.insert (Vector x y') (Is Wall) >> levelmap %=  M.insert (Vector 0 y') (Is Wall))
          genroom v1 v2 whetherVertical = case ((abs .: (*)) `overCoords` (mappend v1 . negateVector $ v2) >= minimumArea) of
            True -> do
                offset <- randomR (-maxOffset, maxOffset)
                let splitDistance = offset + getDim whetherVertical v1 + abs (((-) `on` getDim whetherVertical) v2 v1 `div` 2)
                let splitPosition = setDim whetherVertical v1 splitDistance
                forM_ [(getDim (not whetherVertical) v1)..(getDim (not whetherVertical) v2)] $ \i -> do
                    levelmap %= M.insert (setDim (not whetherVertical) splitPosition i) (Is Wall)
                genroom v1 (setDim whetherVertical v2 splitDistance) (not whetherVertical)
                genroom splitPosition v2 (not whetherVertical)
            False -> rooms %= ((v1,v2):)
