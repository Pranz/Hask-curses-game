
module Main where

import Control.Monad.Trans.State
import UI.NCurses
import qualified Game

main ::IO ()
main = do
    runCurses (execStateT Game.initGame Game.initWorld)
    return ()

--main :: IO ()
--main = runCurses $ do
--    setEcho False
--    w <-defaultWindow
--    updateWindow w $ do
--        moveCursor 1 10
--        drawString "Hello world!"
--        moveCursor 3 10
--        drawString "(press q to quit)"
--        moveCursor 0 0
--    render
--    waitFor w (\ev ->ev == EventCharacter 'q' ||ev ==EventCharacter 'Q')
--
--waitFor :: Window ->(Event ->Command) ->Curses ()
--waitFor w p = loop where
--    loop = do
--        ev <- getEvent w Nothing
--        case ev of
--            Nothing -> loop
--            Just ev' -> if p ev' then return () else