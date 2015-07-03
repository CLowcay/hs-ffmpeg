-- -*- haskell -*- 
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Module 'Media.FFMpeg.UtilEnums_' implements 
    enumerations from libavutil
   
   (c) 2009 Vasyl Pasternak
 -}


module Media.FFMpeg.UtilEnums_
    (
     PixelFormat (..)
    ) where

#include "ffmpeg.h"
#include "macros.hsc2hs.h"

-- |PixelFormat enumeration
#{begin_enum PixelFormat, PIX_FMT_NONE}
#{add_enum PIX_FMT_YUV420P}
#{add_enum PIX_FMT_YUYV422}
#{add_enum PIX_FMT_RGB24}
#{add_enum PIX_FMT_BGR24}
#{add_enum PIX_FMT_YUV422P}
#{add_enum PIX_FMT_YUV444P}
#{add_enum PIX_FMT_YUV410P}
#{add_enum PIX_FMT_YUV411P}
#{add_enum PIX_FMT_GRAY8}
#{add_enum PIX_FMT_MONOWHITE}
#{add_enum PIX_FMT_MONOBLACK}
#{add_enum PIX_FMT_PAL8}
#{add_enum PIX_FMT_YUVJ420P}
#{add_enum PIX_FMT_YUVJ422P}
#{add_enum PIX_FMT_YUVJ444P}
#{add_enum PIX_FMT_XVMC_MPEG2_MC}
#{add_enum PIX_FMT_XVMC_MPEG2_IDCT}
#{add_enum PIX_FMT_UYVY422}
#{add_enum PIX_FMT_UYYVYY411}
#{add_enum PIX_FMT_BGR8}
#{add_enum PIX_FMT_BGR4}
#{add_enum PIX_FMT_BGR4_BYTE}
#{add_enum PIX_FMT_RGB8}
#{add_enum PIX_FMT_RGB4}
#{add_enum PIX_FMT_RGB4_BYTE}
#{add_enum PIX_FMT_NV12}
#{add_enum PIX_FMT_NV21}
#{add_enum PIX_FMT_ARGB}
#{add_enum PIX_FMT_RGBA}
#{add_enum PIX_FMT_ABGR}
#{add_enum PIX_FMT_BGRA}
#{add_enum PIX_FMT_GRAY16BE}
#{add_enum PIX_FMT_GRAY16LE}
-- #{add_enum PIX_FMT_YUV440P}
-- #{add_enum PIX_FMT_YUVJ440P}
-- #{add_enum PIX_FMT_YUVA420P}
-- #{add_enum PIX_FMT_VDPAU_H264}
-- #{add_enum PIX_FMT_VDPAU_MPEG1}
-- #{add_enum PIX_FMT_VDPAU_MPEG2}
-- #{add_enum PIX_FMT_VDPAU_WMV3}
-- #{add_enum PIX_FMT_VDPAU_VC1}
-- #{add_enum PIX_FMT_RGB48BE}
-- #{add_enum PIX_FMT_RGB48LE}
-- #{add_enum PIX_FMT_RGB565BE}
-- #{add_enum PIX_FMT_RGB565LE}
-- #{add_enum PIX_FMT_RGB555BE}
-- #{add_enum PIX_FMT_RGB555LE}
-- #{add_enum PIX_FMT_BGR565BE}
-- #{add_enum PIX_FMT_BGR565LE}
-- #{add_enum PIX_FMT_BGR555BE}
-- #{add_enum PIX_FMT_BGR555LE}
-- #{add_enum PIX_FMT_VAAPI_MOCO}
-- #{add_enum PIX_FMT_VAAPI_IDCT}
-- #{add_enum PIX_FMT_VAAPI_VLD}
#{add_enum PIX_FMT_NB}
#{add_enum PIX_FMT_RGB32}
#{add_enum PIX_FMT_RGB32_1}
#{add_enum PIX_FMT_BGR32}
#{add_enum PIX_FMT_BGR32_1}
#{add_enum PIX_FMT_GRAY16}
-- #{add_enum PIX_FMT_RGB48}
#{add_enum PIX_FMT_RGB565}
#{add_enum PIX_FMT_RGB555}
#{add_enum PIX_FMT_BGR565}
#{add_enum PIX_FMT_BGR555}
#{end_enum "Eq, Ord, Show"}
