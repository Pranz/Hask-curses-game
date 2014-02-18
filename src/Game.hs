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
import Control.Monad
import Control.Lens
import Control.Monad
import Data.Function (on)
import Data.Monoid
import Data.Maybe
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
  { _levelmap        :: M.Map Vector (Is MapObject)
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
    blocks = to.const $ True

instance Representable Wall where
    char  = to (const '#')
    color = undefined

instance Static Wall where
    blocks = to.const $ True
    
instance MapObject Wall

initmap   = M.fromList
  [(Vector 5 5, Is Wall)
  ,(Vector 4 5, Is Wall)
  ,(Vector 7 3, Is Wall)
  ]
ent       = Entity (Vector 5 3) '@' undefined
initWorld = World initmap ent "Hej"

initGame :: StateT World Curses ()
initGame = do
  w <-lift $ do
    setEcho False
    defaultWindow
  loop w

loop ::Window -> StateT World Curses ()
loop w = do
  heroLocation  <- use $ hero.location
  heroChar      <- use $ hero.char
  str           <- use testString
  levelmap'     <- use levelmap
  
  let statics = M.toList levelmap'
  --rendering
  lift $ (updateWindow w $ do
    --hero
    moveCursorToVector heroLocation
    drawString.return $ heroChar
    --statics
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
  if isNothing (mfilter (== Meta Quit) command) then loop w else return ()

moveCursorToVector :: Vector -> Update ()
moveCursorToVector = overCoords (flip moveCursor `on` fromIntegral)

guardfilter p x = (guard.p) x >> return x

isValidPosition :: Vector -> Bool
isValidPosition = overCoords (on (&&) (>= 0))