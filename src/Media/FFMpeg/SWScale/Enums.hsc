{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

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
	pattern SwsFastBilinear,
	pattern SwsBicubic,
	pattern SwsX,
	pattern SwsPoint,
	pattern SwsArea,
	pattern SwsBicublin,
	pattern SwsGauss,
	pattern SwsSinc,
	pattern SwsLanczos,
	pattern SwsSpline,

	ScaleFlag,
	pattern SwsPrintInfo,
	pattern SwsFullChrHInt,
	pattern SwsFullChrHInp,
	pattern SwsDirectBgr,
	pattern SwsAccurateRnd,
	pattern SwsBitexact,
	pattern SwsErrorDiffusion,

	ChromaDrop(..),

	pattern SWSMaxReduceCutoff,

	SwsColorSpace,
	pattern SwsCsItu709,
	pattern SwsCsFcc,
	pattern SwsCsItu601,
	pattern SwsCsItu624,
	pattern SwsCsSmpte170m,
	pattern SwsCsSmpte240m,
	pattern SwsCsDefault
) where

import Foreign.C.Types

import Media.FFMpeg.Internal.Common

#include "ffmpeg.h"

-- | Scaling algorithm flags
newtype ScaleAlgorithm = ScaleAlgorithm CInt deriving (Eq, Show, CEnum)
pattern SwsFastBilinear = ScaleAlgorithm (#{const SWS_FAST_BILINEAR})
pattern SwsBicubic = ScaleAlgorithm (#{const SWS_BICUBIC})
pattern SwsX = ScaleAlgorithm (#{const SWS_X})
pattern SwsPoint = ScaleAlgorithm (#{const SWS_POINT})
pattern SwsArea = ScaleAlgorithm (#{const SWS_AREA})
pattern SwsBicublin = ScaleAlgorithm (#{const SWS_BICUBLIN})
pattern SwsGauss = ScaleAlgorithm (#{const SWS_GAUSS})
pattern SwsSinc = ScaleAlgorithm (#{const SWS_SINC})
pattern SwsLanczos = ScaleAlgorithm (#{const SWS_LANCZOS})
pattern SwsSpline = ScaleAlgorithm (#{const SWS_SPLINE})

-- | Scaling options flags
newtype ScaleFlag = ScaleFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern SwsPrintInfo = ScaleFlag (#{const SWS_PRINT_INFO})
pattern SwsFullChrHInt = ScaleFlag (#{const SWS_FULL_CHR_H_INT})
pattern SwsFullChrHInp = ScaleFlag (#{const SWS_FULL_CHR_H_INP})
pattern SwsDirectBgr = ScaleFlag (#{const SWS_DIRECT_BGR})
pattern SwsAccurateRnd = ScaleFlag (#{const SWS_ACCURATE_RND})
pattern SwsBitexact = ScaleFlag (#{const SWS_BITEXACT})
pattern SwsErrorDiffusion = ScaleFlag (#{const SWS_ERROR_DIFFUSION})

-- | Chroma drop
data ChromaDrop =
	SwsSrcVChrDrop0 |
	SwsSrcVChrDrop1 |
	SwsSrcVChrDrop2 |
	SwsSrcVChrDrop3  deriving (Eq, Show, Ord, Enum)

-- | SWS_MAX_REDUCE_CUTOFF constant
pattern SWSMaxReduceCutoff = #{const SWS_MAX_REDUCE_CUTOFF}

-- | SWS_CS_ constants
newtype SwsColorSpace = SwsColorSpace CInt deriving (Eq, Show, CEnum)
pattern SwsCsItu709 = SwsColorSpace (#{const SWS_CS_ITU709})
pattern SwsCsFcc = SwsColorSpace (#{const SWS_CS_FCC})
pattern SwsCsItu601 = SwsColorSpace (#{const SWS_CS_ITU601})
pattern SwsCsItu624 = SwsColorSpace (#{const SWS_CS_ITU624})
pattern SwsCsSmpte170m = SwsColorSpace (#{const SWS_CS_SMPTE170M})
pattern SwsCsSmpte240m = SwsColorSpace (#{const SWS_CS_SMPTE240M})
pattern SwsCsDefault = SwsColorSpace (#{const SWS_CS_DEFAULT})

