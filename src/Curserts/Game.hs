{-# LANGUAGE TemplateHaskell           #-}
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
  ) where

import Prelude hiding (Right, Left)
import UI.NCurses
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import Control.Lens
import Control.Applicative (liftA2)
import Data.Function (on)
import Data.Monoid
import Data.Maybe
import qualified System.Random as R
import qualified Data.Map as M
import qualified Data.Foldable as F

import Curserts.Classes.Classes
import Curserts.Input.Input
import Curserts.Geometry
import Curserts.Util

window_width :: Integer
window_width  = fromInteger 60
window_height :: Integer
window_height = fromInteger 24

greenOnBlack = newColorID ColorGreen ColorBlack 1


data Wall = Wall
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
  , _randomgen  :: R.StdGen
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

initmap   = M.fromList
  [(Vector 5 5, Is Wall)
  ,(Vector 4 5, Is Wall)
  ,(Vector 7 3, Is Wall)
  ]
ent       = Entity (Vector 5 3) '@' undefined
initWorld = World initmap ent "Hej" [] 0

initialize :: Window -> StateT World Curses ()
initialize w = do
  grnblack <- lift greenOnBlack
  hero.color .= grnblack

update :: Window -> StateT World Curses ()
update w = do
  levelmap'     <- use levelmap
  
  let statics = M.toList levelmap'
  
  turnNumber += 1
  use (hero.location) >>= appendToLog "Position: "
  
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
      moveCursorToVector position
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
  
random :: R.Random a => StateT World Curses a
random = do
  (myrandom, newgen) <- use (randomgen.to R.random)
  randomgen .= newgen
  return myrandom
  
randomR :: R.Random a => (a,a) -> StateT World Curses a
randomR bounds = do
  (myrandom, newgen) <- use (randomgen.to (R.randomR bounds))
  randomgen .= newgen
  return myrandom

randoms :: R.Random a => Int -> StateT World Curses [a]
randoms n = do
  sequence. take n. repeat $ random

randomRs n = do
  sequence. take n. repeat. randomR
  
