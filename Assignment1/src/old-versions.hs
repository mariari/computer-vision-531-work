-- I round up to Word16's
-- also note that this algorithm can be made completely general on Integrals and not just Word8's however, I want speed
--blurSepX :: Matrix Word8 -> Matrix Word8
--blurSepX :: (Integral a, Fractional b) => Matrix a -> Matrix b
blurSepX mat =  withWord16 (* gausblur) <$> extracted
  where
    clampU    = colVector $ getCol 1           mat -- this gives us the clamp border effect
    clampD    = colVector $ getCol (ncols mat) mat
    buffered  = (clampU <|> clampU) <|> mat <|> (clampD <|> clampD)
    extracted = extractWindows 1 5 buffered
    gausblur  = fromList 5 1 gausianConst

--blurSepY :: Matrix Word8 -> Matrix Word8
--blurSepY :: (Num a, Fractional b) => Matrix a -> Matrix b
blurSepY mat = withWord16 (gausblur *) <$> extracted
  where
    clampL    = rowVector $ getRow 1           mat
    clampR    = rowVector $ getRow (nrows mat) mat
    buffered  = (clampL <-> clampL) <-> mat <-> (clampR <-> clampR)
    extracted = extractWindows 5 1 buffered
    gausblur  = fromList 1 5 gausianConst

-- does computation in word 16, before going back down to Word8
--withWord16 :: (Matrix Word16 -> Matrix Word16) -> Matrix Word16 -> Word16
withWord16 f mat = round $ (fromIntegral . sum $ f mat16) / 16
  where
    mat16 = fromIntegral <$> mat :: Matrix Word16

blur :: (Integral a, Integral b) => Matrix a -> Matrix b
blur = blurSepY . blurSepX
