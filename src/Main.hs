
module Main where

import Control.Monad.Trans.State
import System.Random
import UI.NCurses
import qualified Curserts.Game as G

main :: IO ()
main = do
	initialRandomGen <- getStdGen
	putStrLn ("Seed: " ++ show initialRandomGen)
	world <- runCurses $ do
		window <- initCurses
		initialized <- execStateT G.initialize (G.initWorld initialRandomGen)
		execStateT (G.update window) initialized
	mapM_ putStrLn (G._gamelog world)
	return ()

initCurses :: Curses Window
initCurses = do
	setEcho False
	newWindow G.window_height G.window_width 0 0