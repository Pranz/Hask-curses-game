module Input where

import UI.NCurses
import Geometry (Direction(..))
import Prelude hiding (Left, Right)

data MetaCommand = Quit
    deriving (Show, Read, Eq)
data Command = Dir Direction | Meta MetaCommand
    deriving (Show, Read, Eq)
event2command :: Event -> Maybe Command
event2command ev = case ev of
    EventSpecialKey k -> case k of
        KeyA1         -> Just .Dir $ LeftUp
        KeyUpArrow    -> Just .Dir $ Up
        KeyA3         -> Just .Dir $ RightUp
        KeyLeftArrow  -> Just .Dir $ Left
        KeyRightArrow -> Just .Dir $ Right
        KeyC1         -> Just .Dir $ LeftDown
        KeyDownArrow  -> Just .Dir $ Down
        KeyC3         -> Just .Dir $ RightDown
        _             -> Nothing
    EventCharacter c ->case c of
        'q' -> Just .Meta $ Quit
        _   -> Nothing
    _ -> Nothing