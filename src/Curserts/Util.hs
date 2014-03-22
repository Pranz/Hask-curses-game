module Curserts.Util where

import Control.Lens hiding (none)

infixr 8 .:
(.:) = (.).(.)

constLens x = lens (const x) const

loeb :: Functor f => f (f a -> a) -> f a
loeb ff = fa
  where fa = fmap ($ fa) ff

none p = not . any p