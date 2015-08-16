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

	ScaleFlag,
	sws_print_info,
	sws_full_chr_h_int,
	sws_full_chr_h_inp,
	sws_direct_bgr,
	sws_accurate_rnd,
	sws_bitexact,
	sws_error_diffusion,

	ChromaDrop(..),
	sws_max_reduce_cutoff,

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

-- | Scaling algorithm flags
newtype ScaleAlgorithm = ScaleAlgorithm CInt deriving (Eq, Show, CEnum)
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

-- | Scaling options flags
newtype ScaleFlag = ScaleFlag CInt deriving (Eq, Show, CEnum, CFlags)
#{enum ScaleFlag, ScaleFlag,
	sws_print_info = SWS_PRINT_INFO,
	sws_full_chr_h_int = SWS_FULL_CHR_H_INT,
	sws_full_chr_h_inp = SWS_FULL_CHR_H_INP,
	sws_direct_bgr = SWS_DIRECT_BGR,
	sws_accurate_rnd = SWS_ACCURATE_RND,
	sws_bitexact = SWS_BITEXACT,
	sws_error_diffusion = SWS_ERROR_DIFFUSION
}

-- | Chroma drop
data ChromaDrop =
	Sws_src_v_chr_drop_0 |
	Sws_src_v_chr_drop_1 |
	Sws_src_v_chr_drop_2 |
	Sws_src_v_chr_drop_3  deriving (Eq, Show, Ord, Enum)

-- | SWS_MAX_REDUCE_CUTOFF constant
sws_max_reduce_cutoff :: Double
sws_max_reduce_cutoff = #{const SWS_MAX_REDUCE_CUTOFF}

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

