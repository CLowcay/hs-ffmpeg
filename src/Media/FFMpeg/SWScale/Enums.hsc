-- -*- haskell -*- 
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
	Module 'Media.FFMpeg.SWScaleEnums_' implements 
	enumeration from libswscale
		 
	(c) 2009 Vasyl Pasternak
-}

module Media.FFMpeg.SWScale.Enums (
	ScaleAlgorithm,
	sws_fast_bilinear,
	sws_bicubic,
	sws_x,
	sws_point,
	sws_area,
	sws_bicublin,
	sws_gauss,
	sws_sinc,
	sws_lanczos,
	sws_spline
) where

import Data.Bits
import Data.Monoid
import Foreign.C.Types
import Media.FFMpeg.Common

#include "ffmpeg.h"

-- |ScaleAlgorithm Enumeration
newtype ScaleAlgorithm = ScaleAlgorithm {unScaleAlgorithm :: CInt}
	deriving (Eq, Show)
instance CEnum ScaleAlgorithm where
	fromCEnum = unScaleAlgorithm
	toCEnum = ScaleAlgorithm
instance Monoid ScaleAlgorithm where
	mempty = ScaleAlgorithm 0
	(ScaleAlgorithm a) `mappend` (ScaleAlgorithm b) = ScaleAlgorithm$ a .|. b

#{enum ScaleAlgorithm, ScaleAlgorithm,
	sws_fast_bilinear = SWS_FAST_BILINEAR,
	sws_bicubic = SWS_BICUBIC,
	sws_x = SWS_X,
	sws_point = SWS_POINT,
	sws_area = SWS_AREA,
	sws_bicublin = SWS_BICUBLIN,
	sws_gauss = SWS_GAUSS,
	sws_sinc = SWS_SINC,
	sws_lanczos = SWS_LANCZOS,
	sws_spline = SWS_SPLINE
}

