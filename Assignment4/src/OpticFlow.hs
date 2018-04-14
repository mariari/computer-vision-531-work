module OpticFlow
  ( δx
  , δy
  , δt
  ) where

import Filters
import Data.Array.Repa as R
import Data.Array.Repa.Stencil.Dim2
import Data.PriorityQueue.FingerTree as F
import Debug.Trace
import Data.Monoid

infin :: Fractional a => a
infin = 1/0

degrees :: (Ord a, Floating a, Eq a) => a -> a -> a
degrees 0    0   = 0
degrees rise 0
  | rise > 0     = 90
  | otherwise    = 180
degrees rise run
  | rise >= 0 && run > 0 = calc
  | rise >  0 && run < 0 = calc + 90
  | rise <= 0 && run < 0 = calc + 180
  | rise <  0 && run > 0 = calc + 270
  where calc = abs $ atan (rise / run) * 180 / pi


-- add a conversion function that converts degrees into a color, so we can see how things move via colors
-- Don't want to add arrows as that takes too much effort


meanDiff :: (Source r c, Source r2 c, Floating c) => Array r DIM2 c -> Array r2 DIM2 c -> c
meanDiff arr1 = sqrt . (/ fromIntegral (i * j)) . sumAllS . R.zipWith (\x y -> abs (x^2 - y^2)) arr1
  where Z :. i :. j = R.extent arr1

-- n is the size of the window
-- m is the size of the local area
-- So we aren't going to go over the entire area, instead we are going to compute the image m * n + n `div` 2 from the edge
-- and do this for patches of size n
convfn n m img1 img2 = R.fromFunction newSize f
  where
    sideSize        = n `div` 2
    edgeSize        = m * n + sideSize
    Z :. i :. j     = extent img1
    newSize         = ix2 (i `div` edgeSize) (j `div` edgeSize) -- this gives the boundary so we stay inside the image
    f (Z :. x :. y) = comp
      where
        centerX     = edgeSize + x * n
        centerY     = edgeSize + y * n
        fromMid ι κ = ix2 (centerX + ι) (centerY + κ)
        extractImg  = extract (fromMid 0 0) (ix2 n n)
        current     = R.computeUnboxedS $ extractImg img1
        sameSpotOn2 = R.computeUnboxedS $ extractImg img2
        comp | current == sameSpotOn2 = 0
             | otherwise              = uncurry degrees added
        -- if the image moved at all then we have to add everything to a priority queue
        added = (fromIntegral lowestI, fromIntegral lowestJ)
          where
            (lowestI, lowestJ) = peek $ foldr insertPQ empty allspots
            allspots           = (,) <$> [negate n*m .. n*m] <*> [negate n*m .. n*m] -- get all points
            insertPQ (ι,κ)     = add diff (ι,κ)
              where
                diff = meanDiff current (extract (fromMid ι κ) (ix2 n n) img2)

peek :: Ord k => PQueue k t -> t
peek = value . minView
  where
    value (Just (a,_)) = a
    value Nothing      = error "didn't bounds check"

data WindowSize = Window3
                | Window5
                | Window7

δx :: (Num b, Source r b) => WindowSize -> Array r DIM2 b -> Array PC5 DIM2 b
δx Window3 = sobelX
δx Window5 = sobelX5
δx Window7 = sobelX7

δy :: (Num b, Source r b) => WindowSize -> Array r DIM2 b -> Array PC5 DIM2 b
δy Window3 = sobelY
δy Window5 = sobelY5
δy Window7 = sobelY7

δt :: (Source r1 c, Source r2 c, Num c) => Array r1 DIM2 c -> Array r2 DIM2 c -> Array D DIM2 c
δt = (-^)

ab :: (Num e, Source r1 e, Source r e)
   => WindowSize
   -> Array r DIM2 e
   -> Array r1 DIM2 e
   -> (Array D DIM2 e, Array D DIM1 e)
ab windowSize img img2 = (diffb, difft)
  where (_ :. i :. j) = R.extent img
        diffb = reshape (ix2 (i*j) 2) $ interleave2 diffx diffy
        diffx = op $ δx windowSize
        diffy = op $ δy windowSize
        difft = op $ R.map negate . δt img2
        op f  = reshape (ix1 (i*j)) (f img)