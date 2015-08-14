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
	sws_spline,

	SwsColorSpace,
	sws_cs_itu709,
	sws_cs_fcc,
	sws_cs_itu601,
	sws_cs_itu624,
	sws_cs_smpte170m,
	sws_cs_smpte240m,
	sws_cs_default
) where

import Foreign.C.Types

import Media.FFMpeg.Internal.Common

#include "ffmpeg.h"

-- | ScaleAlgorithm Enumeration
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

{-
#define 	SWS_SRC_V_CHR_DROP_MASK   0x30000
#define 	SWS_SRC_V_CHR_DROP_SHIFT   16
 
#define 	SWS_PARAM_DEFAULT   123456
 
#define 	SWS_PRINT_INFO          0x1000
#define 	SWS_FULL_CHR_H_INT      0x2000
#define 	SWS_FULL_CHR_H_INP      0x4000
#define 	SWS_DIRECT_BGR          0x8000
#define 	SWS_ACCURATE_RND       0x40000
#define 	SWS_BITEXACT           0x80000
#define 	SWS_ERROR_DIFFUSION   0x800000
 
#define 	SWS_MAX_REDUCE_CUTOFF   0.002
-}

-- | SWS_CS_ constants
newtype SwsColorSpace = SwsColorSpace CInt deriving (Eq, Show, CEnum)
#{enum SwsColorSpace, SwsColorSpace,
	sws_cs_itu709 = SWS_CS_ITU709,
	sws_cs_fcc = SWS_CS_FCC,
	sws_cs_itu601 = SWS_CS_ITU601,
	sws_cs_itu624 = SWS_CS_ITU624,
	sws_cs_smpte170m = SWS_CS_SMPTE170M,
	sws_cs_smpte240m = SWS_CS_SMPTE240M,
	sws_cs_default = SWS_CS_DEFAULT
}

