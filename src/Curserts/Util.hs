module Curserts.Util where

import Control.Lens

infixr 8 .:
(.:) = (.).(.)

constLens x = lens (const x) const