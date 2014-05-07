module Curserts.Random where

import qualified System.Random as R
import Curserts.Data
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Lens

random :: (R.Random a, Monad m) => StateT World m a
random = do
  (myrandom, newgen) <- use (randomgen.to R.random)
  randomgen .= newgen
  return myrandom

randomR :: (R.Random a, Monad m) => (a,a) -> StateT World m a
randomR bounds = do
  (myrandom, newgen) <- use (randomgen.to (R.randomR bounds))
  randomgen .= newgen
  return myrandom

randoms :: (R.Random a, Monad m) => Int -> StateT World m [a]
randoms n = do
  sequence. take n. repeat $ random

randomRs n = do
  sequence. take n. repeat. randomR

choice ls = do
  index <- randomR (0,length ls - 1)
  return $ ls !! index
