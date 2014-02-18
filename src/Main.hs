
module Main where

import Control.Monad.Trans.State
import System.Random
import UI.NCurses
import qualified Game

main :: IO ()
main = do
	initialRandomGen <- getStdGen
	putStrLn ("Seed: " ++ show initialRandomGen)
	world <- runCurses $ do
		window <- initCurses
		initialized <- execStateT Game.initialize (Game.initWorld initialRandomGen)
		execStateT (Game.update window) initialized
	mapM_ putStrLn (Game._gamelog world)
	return ()

initCurses :: Curses Window
initCurses = do
	setEcho False
	newWindow Game.window_height Game.window_width 0 0
  
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