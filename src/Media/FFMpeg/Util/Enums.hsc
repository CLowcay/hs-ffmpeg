-- -*- haskell -*- 
{-# LANGUAGE ForeignFunctionInterface #-}

{- |

Module      : 
Description : Enumerations for libavutil
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Enumerations for libavutil.

-}

module Media.FFMpeg.Util.Enums (
	PixelFormat,
	pix_fmt_none,
	pix_fmt_yuv420p,
	pix_fmt_yuyv422,
	pix_fmt_rgb24,
	pix_fmt_bgr24,
	pix_fmt_yuv422p,
	pix_fmt_yuv444p,
	pix_fmt_yuv410p,
	pix_fmt_yuv411p,
	pix_fmt_gray8,
	pix_fmt_monowhite,
	pix_fmt_monoblack,
	pix_fmt_pal8,
	pix_fmt_yuvj420p,
	pix_fmt_yuvj422p,
	pix_fmt_yuvj444p,
	pix_fmt_xvmc_mpeg2_mc,
	pix_fmt_xvmc_mpeg2_idct,
	pix_fmt_uyvy422,
	pix_fmt_uyyvyy411,
	pix_fmt_bgr8,
	pix_fmt_bgr4,
	pix_fmt_bgr4_byte,
	pix_fmt_rgb8,
	pix_fmt_rgb4,
	pix_fmt_rgb4_byte,
	pix_fmt_nv12,
	pix_fmt_nv21,
	pix_fmt_argb,
	pix_fmt_rgba,
	pix_fmt_abgr,
	pix_fmt_bgra,
	pix_fmt_gray16be,
	pix_fmt_gray16le,
	pix_fmt_nb,
	pix_fmt_rgb32,
	pix_fmt_rgb32_1,
	pix_fmt_bgr32,
	pix_fmt_bgr32_1,
	pix_fmt_gray16,
	pix_fmt_rgb565,
	pix_fmt_rgb555,
	pix_fmt_bgr565,
	pix_fmt_bgr555
) where

import Foreign.C.Types
import Media.FFMpeg.Common

#include "ffmpeg.h"

-- |PixelFormat enumeration
newtype PixelFormat = PixelFormat {unPixelFormat :: CInt} deriving (Eq, Show)
instance CEnum PixelFormat where
	fromCEnum = unPixelFormat
	toCEnum = PixelFormat

#{enum PixelFormat, PixelFormat,
	pix_fmt_none = PIX_FMT_NONE,
	pix_fmt_yuv420p = PIX_FMT_YUV420P,
	pix_fmt_yuyv422 = PIX_FMT_YUYV422,
	pix_fmt_rgb24 = PIX_FMT_RGB24,
	pix_fmt_bgr24 = PIX_FMT_BGR24,
	pix_fmt_yuv422p = PIX_FMT_YUV422P,
	pix_fmt_yuv444p = PIX_FMT_YUV444P,
	pix_fmt_yuv410p = PIX_FMT_YUV410P,
	pix_fmt_yuv411p = PIX_FMT_YUV411P,
	pix_fmt_gray8 = PIX_FMT_GRAY8,
	pix_fmt_monowhite = PIX_FMT_MONOWHITE,
	pix_fmt_monoblack = PIX_FMT_MONOBLACK,
	pix_fmt_pal8 = PIX_FMT_PAL8,
	pix_fmt_yuvj420p = PIX_FMT_YUVJ420P,
	pix_fmt_yuvj422p = PIX_FMT_YUVJ422P,
	pix_fmt_yuvj444p = PIX_FMT_YUVJ444P,
	pix_fmt_xvmc_mpeg2_mc = PIX_FMT_XVMC_MPEG2_MC,
	pix_fmt_xvmc_mpeg2_idct = PIX_FMT_XVMC_MPEG2_IDCT,
	pix_fmt_uyvy422 = PIX_FMT_UYVY422,
	pix_fmt_uyyvyy411 = PIX_FMT_UYYVYY411,
	pix_fmt_bgr8 = PIX_FMT_BGR8,
	pix_fmt_bgr4 = PIX_FMT_BGR4,
	pix_fmt_bgr4_byte = PIX_FMT_BGR4_BYTE,
	pix_fmt_rgb8 = PIX_FMT_RGB8,
	pix_fmt_rgb4 = PIX_FMT_RGB4,
	pix_fmt_rgb4_byte = PIX_FMT_RGB4_BYTE,
	pix_fmt_nv12 = PIX_FMT_NV12,
	pix_fmt_nv21 = PIX_FMT_NV21,
	pix_fmt_argb = PIX_FMT_ARGB,
	pix_fmt_rgba = PIX_FMT_RGBA,
	pix_fmt_abgr = PIX_FMT_ABGR,
	pix_fmt_bgra = PIX_FMT_BGRA,
	pix_fmt_gray16be = PIX_FMT_GRAY16BE,
	pix_fmt_gray16le = PIX_FMT_GRAY16LE,
	pix_fmt_nb = PIX_FMT_NB,
	pix_fmt_rgb32 = PIX_FMT_RGB32,
	pix_fmt_rgb32_1 = PIX_FMT_RGB32_1,
	pix_fmt_bgr32 = PIX_FMT_BGR32,
	pix_fmt_bgr32_1 = PIX_FMT_BGR32_1,
	pix_fmt_gray16 = PIX_FMT_GRAY16,
	pix_fmt_rgb565 = PIX_FMT_RGB565,
	pix_fmt_rgb555 = PIX_FMT_RGB555,
	pix_fmt_bgr565 = PIX_FMT_BGR565,
	pix_fmt_bgr555 = PIX_FMT_BGR555
}

-- PIX_FMT_YUV440P
-- PIX_FMT_YUVJ440P
-- PIX_FMT_YUVA420P
-- PIX_FMT_VDPAU_H264
-- PIX_FMT_VDPAU_MPEG1
-- PIX_FMT_VDPAU_MPEG2
-- PIX_FMT_VDPAU_WMV3
-- PIX_FMT_VDPAU_VC1
-- PIX_FMT_RGB48BE
-- PIX_FMT_RGB48LE
-- PIX_FMT_RGB565BE
-- PIX_FMT_RGB565LE
-- PIX_FMT_RGB555BE
-- PIX_FMT_RGB555LE
-- PIX_FMT_BGR565BE
-- PIX_FMT_BGR565LE
-- PIX_FMT_BGR555BE
-- PIX_FMT_BGR555LE
-- PIX_FMT_VAAPI_MOCO
-- PIX_FMT_VAAPI_IDCT
-- PIX_FMT_VAAPI_VLD
-- PIX_FMT_RGB48

