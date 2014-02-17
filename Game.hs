{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds           #-}

module Game 
  ( loop
  , initWorld
  , World
  , initGame
  ) where

import Prelude hiding (Right, Left)
import UI.NCurses
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Monoid
import Control.Lens
import Control.Monad
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Foldable as F

import Classes
import Geometry
import Input

data Wall = Wall
data Entity = Entity
  { _entPosition :: Vector
  , _entChar     :: Char
  , _entColor    :: Curses ColorID
  }
makeLenses ''Entity
data World = World
  { _levelmap        :: M.Map Vector (Is' Representable Static)
  , _hero       :: Entity
  , _testString :: String
  }
makeLenses ''World

instance Locatable Entity where
    location = entPosition

instance Representable Entity where
    char  = entChar
    color = entColor

instance Static Entity where
    blocks = const True

instance Representable Wall where
    char  = to (const '#')
    color = undefined

instance Static Wall where
    blocks = const True

ent       = Entity (Vector 5 3) '@' undefined
initWorld = World (M.singleton mempty $ Is' Wall) ent "Hej"

initGame :: StateT World Curses ()
initGame = do
  w <-lift $ do
    setEcho False
    defaultWindow
  loop w

loop ::Window -> StateT World Curses ()
loop w = do
  heroLocation  <- use $ hero.location
  heroChar <- use $ hero.char
  str      <- use testString
  levelmap'   <- use levelmap
  
  let statics = M.toList levelmap'
  --rendering
  lift $ (updateWindow w $ do
    --hero
    moveCursorToVector heroLocation
    drawString.return $ heroChar
    --statics
    F.forM_ statics $ \(position, object) -> do
      moveCursorToVector position
      drawString.return $ object^.to (conmap' (^.char))
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
      Dir dir -> hero.location <>= direction2vector dir
      _ -> return ()
  if (mfilter (== Meta Quit) command) == Nothing then loop w else return ()

moveCursorToVector :: Vector -> Update ()
moveCursorToVector = overCoords (flip moveCursor `on` fromIntegral)