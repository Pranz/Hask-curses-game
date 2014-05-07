{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds           #-}

module Curserts.Game
  ( update
  , initWorld
  , World
  , window_width
  , window_height
  , _gamelog
  , initialize
  , initLevel
  ) where

import Prelude hiding (Right, Left)
import UI.NCurses
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Control.Lens hiding (Level, none)
import Control.Applicative (liftA2)
import Data.Function (on)
import Data.Monoid
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Foldable as F

import Curserts.Types.Classes
import Curserts.Input.Input
import Curserts.Geometry
import Curserts.Util
import Curserts.Generation.RoomGeneration
import Curserts.Data
import Curserts.Random

window_width :: Integer
window_width  = fromInteger 60
window_height :: Integer
window_height = fromInteger 24

greenOnBlack = newColorID ColorGreen ColorBlack 1

generateList :: Monad m => Int -> ([a] -> StateT s m [a]) -> StateT s m [a]
generateList n f = gen n f []
  where gen 0 _ xs  = return xs
        gen n f xs = f xs >>= gen (n-1) f

expandStateful :: Monad m => Int -> ([a] -> a -> Bool) -> StateT s m a -> StateT s m [a]
expandStateful n p sVal = expand n sVal []
  where expand 0 _        xs = return xs
        expand n stateful xs = do
          mystate <- stateful
          case p xs mystate of
            True  -> expand (n-1) stateful (mystate:xs)
            False -> expand n     stateful xs

initmap   = M.empty
ent       = Entity (Vector 5 3) '@' undefined
initWorld = World initmap ent "Hej" [] 0 []

initialize :: StateT World Curses ()
initialize = do
  grnblack <- lift greenOnBlack
  hero.color .= grnblack
  initLevel
 
initLevel :: StateT World Curses ()
initLevel = generateRoom $ (Vector `on` (pred . pred . fromIntegral)) window_width (window_height - 1)

update :: Window -> StateT World Curses ()
update w = do
  levelmap'     <- use levelmap

  let statics = M.toList levelmap'

  turnNumber += 1
  
  heroLocation  <- use $ hero.location
  heroChar      <- use $ hero.char
  heroColor     <- use $ hero.color
  --rendering
  lift $ (updateWindow w $ do
    setColor heroColor
    --hero
    moveCursorToVector heroLocation
    drawString.return $ heroChar
    --statics
    setColor defaultColorID
    F.forM_ statics $ \(position, object) -> do
      when (isValidPosition position) (moveCursorToVector position)
      drawString.return $ conmap (view char) object
    moveCursor 0 0) >> render
  
  --prompt for input
  ev <- lift $ getEvent w Nothing
  let command = ev >>= event2command
  
  --erase previous position
  lift . updateWindow w $ do
    moveCursorToVector heroLocation
    drawString.return $ ' '
  
  --handle input
  F.forM_ command $ \cmd -> do
    
    case cmd of
      Dir dir -> do
        let newPos                = heroLocation <> direction2vector dir
        let positionIsBlocked pos = (M.lookup pos levelmap')^..traverse.fromConstraint blocks
        let occupied              = (== [True]) $ maybe [True] positionIsBlocked (guardfilter isValidPosition newPos)
        unless occupied $ do
          hero.location .= newPos
      
      _ -> return ()
  if isNothing (mfilter (== Meta Quit) command) then update w else return ()

moveCursorToVector :: Vector -> Update ()
moveCursorToVector = overCoords (flip moveCursor `on` fromIntegral)

guardfilter p x = (guard.p) x >> return x

isValidPosition :: Vector -> Bool
isValidPosition vec@(Vector xx yy) = overCoords (on (&&) (>= 0) ) vec && (fromIntegral xx < window_width) && (fromIntegral (yy+1) < window_height)

appendToLog :: Show a => String -> a -> StateT World Curses ()
appendToLog description value = do
  gamelog %= ((description ++ show value) :)
