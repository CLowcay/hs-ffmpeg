{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Module      : 
Description : Enumerations for libavutil
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Enumerations from libavutil.

-}

module Media.FFMpeg.Util.Enums (
	AVFrameSideDataType,
	av_frame_data_panscan,
	av_frame_data_a53_cc,
	av_frame_data_stereo3d,
	av_frame_data_matrixencoding,
	av_frame_data_downmix_info,
	av_frame_data_replaygain,
	av_frame_data_displaymatrix,
	av_frame_data_afd,
	av_frame_data_motion_vectors,
	av_frame_data_skip_samples,

	av_num_data_pointers,

	AVActiveFormatDescription,
	av_afd_same,
	av_afd_4_3,
	av_afd_16_9,
	av_afd_14_9,
	av_afd_4_3_sp_14_9,
	av_afd_16_9_sp_14_9,
	av_afd_sp_4_3,

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
	pix_fmt_rgb32,
	pix_fmt_rgb32_1,
	pix_fmt_bgr32,
	pix_fmt_bgr32_1,
	pix_fmt_gray16,
	pix_fmt_rgb565,
	pix_fmt_rgb555,
	pix_fmt_bgr565,
	pix_fmt_bgr555,

	AVColorPrimaries,
	avcol_pri_reserved0,
	avcol_pri_bt709,
	avcol_pri_unspecified,
	avcol_pri_reserved,
	avcol_pri_bt470m,
	avcol_pri_bt470bg,
	avcol_pri_smpte170m,
	avcol_pri_smpte240m,
	avcol_pri_film,
	avcol_pri_bt2020,

	AVColorTransferCharacteristic,
	avcol_trc_reserved0,
	avcol_trc_bt709,
	avcol_trc_unspecified,
	avcol_trc_reserved,
	avcol_trc_gamma22,
	avcol_trc_gamma28,
	avcol_trc_smpte170m,
	avcol_trc_smpte240m,
	avcol_trc_linear,
	avcol_trc_log,
	avcol_trc_log_sqrt,
	avcol_trc_iec61966_2_4,
	avcol_trc_bt1361_ecg,
	avcol_trc_iec61966_2_1,
	avcol_trc_bt2020_10,
	avcol_trc_bt2020_12,

	AVColorSpace,
	avcol_spc_rgb,
	avcol_spc_bt709,
	avcol_spc_unspecified,
	avcol_spc_reserved,
	avcol_spc_fcc,
	avcol_spc_bt470bg,
	avcol_spc_smpte170m,
	avcol_spc_smpte240m,
	avcol_spc_ycocg,
	avcol_spc_bt2020_ncl,
	avcol_spc_bt2020_cl,
	avcol_spc_ycgco,

	AVColorRange,
	avcol_range_unspecified,
	avcol_range_mpeg,
	avcol_range_jpeg,

	AVChromaLocation,
	avchroma_loc_unspecified,
	avchroma_loc_left,
	avchroma_loc_center,
	avchroma_loc_topleft,
	avchroma_loc_top,
	avchroma_loc_bottomleft,
	avchroma_loc_bottom,

	AVStereo3DType,
	av_stereo3d_2d,
	av_stereo3d_sidebyside,
	av_stereo3d_topbottom,
	av_stereo3d_framesequence,
	av_stereo3d_checkerboard,
	av_stereo3d_sidebyside_quincunx,
	av_stereo3d_lines,
	av_stereo3d_columns,

	AVStereo3DFlags,
	av_stereo3d_flag_invert,

	AVDownmixType,
	av_downmix_type_unknown,
	av_downmix_type_loro,
	av_downmix_type_ltrt,
	av_downmix_type_dplii
) where

import Foreign.C.Types
import Media.FFMpeg.Internal.Common

#include "ffmpeg.h"

-- | AVFrameSideDataType enum
newtype AVFrameSideDataType = AVFrameSideDataType CInt deriving (Eq, Show, CEnum)
#{enum AVFrameSideDataType, AVFrameSideDataType,
	av_frame_data_panscan = AV_FRAME_DATA_PANSCAN,
	av_frame_data_a53_cc = AV_FRAME_DATA_A53_CC,
	av_frame_data_stereo3d = AV_FRAME_DATA_STEREO3D,
	av_frame_data_matrixencoding = AV_FRAME_DATA_MATRIXENCODING,
	av_frame_data_downmix_info = AV_FRAME_DATA_DOWNMIX_INFO,
	av_frame_data_replaygain = AV_FRAME_DATA_REPLAYGAIN,
	av_frame_data_displaymatrix = AV_FRAME_DATA_DISPLAYMATRIX,
	av_frame_data_afd = AV_FRAME_DATA_AFD,
	av_frame_data_motion_vectors = AV_FRAME_DATA_MOTION_VECTORS,
	av_frame_data_skip_samples = AV_FRAME_DATA_SKIP_SAMPLES
}

-- | AV_NUM_DATA_POINTERS
av_num_data_pointers :: Int
av_num_data_pointers = #{const AV_NUM_DATA_POINTERS}

-- | AVActiveFormatDescription
newtype AVActiveFormatDescription = AVActiveFormatDescription CInt deriving (Eq, Show, CEnum)
#{enum AVActiveFormatDescription, AVActiveFormatDescription,
	av_afd_same = AV_AFD_SAME,
	av_afd_4_3 = AV_AFD_4_3,
	av_afd_16_9 = AV_AFD_16_9,
	av_afd_14_9 = AV_AFD_14_9,
	av_afd_4_3_sp_14_9 = AV_AFD_4_3_SP_14_9,
	av_afd_16_9_sp_14_9 = AV_AFD_16_9_SP_14_9,
	av_afd_sp_4_3 = AV_AFD_SP_4_3
}

-- | PixelFormat enumeration
newtype PixelFormat = PixelFormat CInt deriving (Eq, Show, CEnum)
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

-- | AVColorPrimaries enum
newtype AVColorPrimaries = AVColorPrimaries CInt deriving (Eq, Show, CEnum)
#{enum AVColorPrimaries, AVColorPrimaries,
	avcol_pri_reserved0 = AVCOL_PRI_RESERVED0,
	avcol_pri_bt709 = AVCOL_PRI_BT709,
	avcol_pri_unspecified = AVCOL_PRI_UNSPECIFIED,
	avcol_pri_reserved = AVCOL_PRI_RESERVED,
	avcol_pri_bt470m = AVCOL_PRI_BT470M,
	avcol_pri_bt470bg = AVCOL_PRI_BT470BG,
	avcol_pri_smpte170m = AVCOL_PRI_SMPTE170M,
	avcol_pri_smpte240m = AVCOL_PRI_SMPTE240M,
	avcol_pri_film = AVCOL_PRI_FILM,
	avcol_pri_bt2020 = AVCOL_PRI_BT2020
}

-- | AVColorTransferCharacteristic enum
newtype AVColorTransferCharacteristic = AVColorTransferCharacteristic CInt deriving (Eq, Show, CEnum)
#{enum AVColorTransferCharacteristic, AVColorTransferCharacteristic,
	avcol_trc_reserved0 = AVCOL_TRC_RESERVED0,
	avcol_trc_bt709 = AVCOL_TRC_BT709,
	avcol_trc_unspecified = AVCOL_TRC_UNSPECIFIED,
	avcol_trc_reserved = AVCOL_TRC_RESERVED,
	avcol_trc_gamma22 = AVCOL_TRC_GAMMA22,
	avcol_trc_gamma28 = AVCOL_TRC_GAMMA28,
	avcol_trc_smpte170m = AVCOL_TRC_SMPTE170M,
	avcol_trc_smpte240m = AVCOL_TRC_SMPTE240M,
	avcol_trc_linear = AVCOL_TRC_LINEAR,
	avcol_trc_log = AVCOL_TRC_LOG,
	avcol_trc_log_sqrt = AVCOL_TRC_LOG_SQRT,
	avcol_trc_iec61966_2_4 = AVCOL_TRC_IEC61966_2_4,
	avcol_trc_bt1361_ecg = AVCOL_TRC_BT1361_ECG,
	avcol_trc_iec61966_2_1 = AVCOL_TRC_IEC61966_2_1,
	avcol_trc_bt2020_10 = AVCOL_TRC_BT2020_10,
	avcol_trc_bt2020_12 = AVCOL_TRC_BT2020_12
}

-- | AVColorSpace enum
newtype AVColorSpace = AVColorSpace CInt deriving (Eq, Show, CEnum)
#{enum AVColorSpace, AVColorSpace,
	avcol_spc_rgb = AVCOL_SPC_RGB,
	avcol_spc_bt709 = AVCOL_SPC_BT709,
	avcol_spc_unspecified = AVCOL_SPC_UNSPECIFIED,
	avcol_spc_reserved = AVCOL_SPC_RESERVED,
	avcol_spc_fcc = AVCOL_SPC_FCC,
	avcol_spc_bt470bg = AVCOL_SPC_BT470BG,
	avcol_spc_smpte170m = AVCOL_SPC_SMPTE170M,
	avcol_spc_smpte240m = AVCOL_SPC_SMPTE240M,
	avcol_spc_ycocg = AVCOL_SPC_YCOCG,
	avcol_spc_bt2020_ncl = AVCOL_SPC_BT2020_NCL,
	avcol_spc_bt2020_cl = AVCOL_SPC_BT2020_CL,
	avcol_spc_ycgco = AVCOL_SPC_YCGCO
}

-- | AVColorRange enum
newtype AVColorRange = AVColorRange CInt deriving (Eq, Show, CEnum)
#{enum AVColorRange, AVColorRange,
	avcol_range_unspecified = AVCOL_RANGE_UNSPECIFIED,
	avcol_range_mpeg = AVCOL_RANGE_MPEG,
	avcol_range_jpeg = AVCOL_RANGE_JPEG
}

-- | AVChromaLocation enum
newtype AVChromaLocation = AVChromaLocation CInt deriving (Eq, Show, CEnum)
#{enum AVChromaLocation, AVChromaLocation,
	avchroma_loc_unspecified = AVCHROMA_LOC_UNSPECIFIED,
	avchroma_loc_left = AVCHROMA_LOC_LEFT,
	avchroma_loc_center = AVCHROMA_LOC_CENTER,
	avchroma_loc_topleft = AVCHROMA_LOC_TOPLEFT,
	avchroma_loc_top = AVCHROMA_LOC_TOP,
	avchroma_loc_bottomleft = AVCHROMA_LOC_BOTTOMLEFT,
	avchroma_loc_bottom = AVCHROMA_LOC_BOTTOM
}

-- | AVStereo3DType enum
newtype AVStereo3DType = AVStereo3DType CInt deriving (Eq, Show, CEnum)
#{enum AVStereo3DType, AVStereo3DType,
	av_stereo3d_2d = AV_STEREO3D_2D,
	av_stereo3d_sidebyside = AV_STEREO3D_SIDEBYSIDE,
	av_stereo3d_topbottom = AV_STEREO3D_TOPBOTTOM,
	av_stereo3d_framesequence = AV_STEREO3D_FRAMESEQUENCE,
	av_stereo3d_checkerboard = AV_STEREO3D_CHECKERBOARD,
	av_stereo3d_sidebyside_quincunx = AV_STEREO3D_SIDEBYSIDE_QUINCUNX,
	av_stereo3d_lines = AV_STEREO3D_LINES,
	av_stereo3d_columns = AV_STEREO3D_COLUMNS
}

-- | Flags for the AVStereo3D struct
newtype AVStereo3DFlags = AVStereo3DFlags CInt deriving (Eq, Show, CEnum, CFlags)
#{enum AVStereo3DFlags, AVStereo3DFlags,
	av_stereo3d_flag_invert = AV_STEREO3D_FLAG_INVERT
}

-- | AVDownmixType enum
newtype AVDownmixType = AVDownmixType CInt deriving (Eq, Show, CEnum)
#{enum AVDownmixType, AVDownmixType,
	av_downmix_type_unknown = AV_DOWNMIX_TYPE_UNKNOWN,
	av_downmix_type_loro = AV_DOWNMIX_TYPE_LORO,
	av_downmix_type_ltrt = AV_DOWNMIX_TYPE_LTRT,
	av_downmix_type_dplii = AV_DOWNMIX_TYPE_DPLII
}

