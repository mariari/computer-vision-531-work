

import           Data.Matrix  as M
import           Control.Lens as L
import           Data.Monoid
import qualified Data.Vector as V
import           Codec.Picture
import           Data.Foldable as F
import qualified Data.Vector.Storable as VS
import           Codec.Picture.Types
import           Matrix.Extras

matrix310 :: (Num a, Enum a) => Matrix a
matrix310 = fromLists [[45,60,98,127,132,133,137,133]
                      ,[46,65,98,123,126,128,131,133]
                      ,[47,65,96,115,119,123,135,137]
                      ,[47,63,91,107,113,122,138,134]]

fromBookOffset = fromList 3 3 $ replicate 4 0.1 <> (0.2 : repeat 0.1)


x = matrix310 ⊕ fromBookOffset