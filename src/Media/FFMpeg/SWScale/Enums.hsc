{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Description : Enumerations for libswscale
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Enumerations for libswscale.

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

import Foreign.C.Types

import Media.FFMpeg.Internal.Common

#include "ffmpeg.h"

-- |ScaleAlgorithm Enumeration
newtype ScaleAlgorithm = ScaleAlgorithm CInt deriving (Eq, Show, CEnum, CFlags)

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

