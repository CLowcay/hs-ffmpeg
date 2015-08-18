{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

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
	pattern AVFrameDataTypePanscan,
	pattern AVFrameDataTypeA53CC,
	pattern AVFrameDataTypeStereo3d,
	pattern AVFrameDataTypeMatrixEncoding,
	pattern AVFrameDataTypeDownmixInfo,
	pattern AVFrameDataTypeReplayGain,
	pattern AVFrameDataTypeDisplayMatrix,
	pattern AVFrameDataTypeAfd,
	pattern AVFrameDataTypeMotionVectors,
	pattern AVFrameDataTypeSkipSamples,

	pattern AVNumDataPointers,

	AVActiveFormatDescription,
	pattern AVAfdSame,
	pattern AVAfd4_3,
	pattern AVAfd16_9,
	pattern AVAfd14_9,
	pattern AVAfd4_3_SP_14_9,
	pattern AVAfd16_9_SP_14_9,
	pattern AVAfdSp_4_3,

	PixelFormat,
	pattern PixFmtNone,
	pattern PixFmtYuv420p,
	pattern PixFmtYuyv422,
	pattern PixFmtRgb24,
	pattern PixFmtBgr24,
	pattern PixFmtYuv422p,
	pattern PixFmtYuv444p,
	pattern PixFmtYuv410p,
	pattern PixFmtYuv411p,
	pattern PixFmtGray8,
	pattern PixFmtMonowhite,
	pattern PixFmtMonoblack,
	pattern PixFmtPal8,
	pattern PixFmtYuvj420p,
	pattern PixFmtYuvj422p,
	pattern PixFmtYuvj444p,
	pattern PixFmtXvmcMpeg2Mc,
	pattern PixFmtXvmcMpeg2Idct,
	pattern PixFmtUyvy422,
	pattern PixFmtUyyvyy411,
	pattern PixFmtBgr8,
	pattern PixFmtBgr4,
	pattern PixFmtBgr4Byte,
	pattern PixFmtRgb8,
	pattern PixFmtRgb4,
	pattern PixFmtRgb4Byte,
	pattern PixFmtNv12,
	pattern PixFmtNv21,
	pattern PixFmtArgb,
	pattern PixFmtRgba,
	pattern PixFmtAbgr,
	pattern PixFmtBgra,
	pattern PixFmtGray16be,
	pattern PixFmtGray16le,
	pattern PixFmtRgb32,
	pattern PixFmtRgb32_1,
	pattern PixFmtBgr32,
	pattern PixFmtBgr32_1,
	pattern PixFmtGray16,
	pattern PixFmtRgb565,
	pattern PixFmtRgb555,
	pattern PixFmtBgr565,
	pattern PixFmtBgr555,

	AVColorPrimaries,
	pattern AVColPriReserved0,
	pattern AVColPriBt709,
	pattern AVColPriUnspecified,
	pattern AVColPriReserved,
	pattern AVColPriBt470m,
	pattern AVColPriBt470bg,
	pattern AVColPriSmpte170m,
	pattern AVColPriSmpte240m,
	pattern AVColPriFilm,
	pattern AVColPriBt2020,

	AVColorTransferCharacteristic,
	pattern AVColTrcReserved0,
	pattern AVColTrcBt709,
	pattern AVColTrcUnspecified,
	pattern AVColTrcReserved,
	pattern AVColTrcGamma22,
	pattern AVColTrcGamma28,
	pattern AVColTrcSmpte170m,
	pattern AVColTrcSmpte240m,
	pattern AVColTrcLinear,
	pattern AVColTrcLog,
	pattern AVColTrcLog_sqrt,
	pattern AVColTrcIec61966_2_4,
	pattern AVColTrcBt1361Ecg,
	pattern AVColTrcIec61966_2_1,
	pattern AVColTrcBt2020_10,
	pattern AVColTrcBt2020_12,

	AVColorSpace,
	pattern AVColSpcRgb,
	pattern AVColSpcBt709,
	pattern AVColSpcUnspecified,
	pattern AVColSpcReserved,
	pattern AVColSpcFcc,
	pattern AVColSpcBt470bg,
	pattern AVColSpcSmpte170m,
	pattern AVColSpcSmpte240m,
	pattern AVColSpcYcocg,
	pattern AVColSpcBt2020_ncl,
	pattern AVColSpcBt2020_cl,
	pattern AVColSpcYcgco,

	AVColorRange,
	pattern AVColRangeUnspecified,
	pattern AVColRangeMpeg,
	pattern AVColRangeJpeg,

	AVChromaLocation,
	pattern AVChromaLocUnspecified,
	pattern AVChromaLocLeft,
	pattern AVChromaLocCenter,
	pattern AVChromaLocTopleft,
	pattern AVChromaLocTop,
	pattern AVChromaLocBottomleft,
	pattern AVChromaLocBottom,

	AVStereo3DType,
	pattern AVStereo3d2d,
	pattern AVStereo3dSidebyside,
	pattern AVStereo3dTopbottom,
	pattern AVStereo3dFramesequence,
	pattern AVStereo3dCheckerboard,
	pattern AVStereo3dSidebysideQuincunx,
	pattern AVStereo3dLines,
	pattern AVStereo3dColumns,

	AVStereo3DFlags,
	pattern AVStereo3dFlagInvert,

	AVDownmixType,
	pattern AVDownmixTypeUnknown,
	pattern AVDownmixTypeLoro,
	pattern AVDownmixTypeLtrt,
	pattern AVDownmixTypeDplii,

	AVOptType,
	pattern AVOptTypeFlags,
	pattern AVOptTypeInt,
	pattern AVOptTypeInt64,
	pattern AVOptTypeDouble,
	pattern AVOptTypeFloat,
	pattern AVOptTypeString,
	pattern AVOptTypeRational,
	pattern AVOptTypeBinary,
	pattern AVOptTypeDict,
	pattern AVOptTypeConst,
	pattern AVOptTypeImageSize,
	pattern AVOptTypePixelFmt,
	pattern AVOptTypeSampleFmt,
	pattern AVOptTypeVideoRate,
	pattern AVOptTypeDuration,
	pattern AVOptTypeColor,
	pattern AVOptTypeChannelLayout,

	AVOptionFlags,
	pattern AVOptFlagEncodingParam,
	pattern AVOptFlagDecodingParam,
	pattern AVOptFlagAudioParam,
	pattern AVOptFlagVideoParam,
	pattern AVOptFlagSubtitleParam,
	pattern AVOptFlagExport,
	pattern AVOptFlagReadonly,
	pattern AVOptFlagFilteringParam,

	AVOptionSearchFlags,
	pattern AVOptSearchChildren,
	pattern AVOptSearchFakeObj,

	AVPictureType,
	pattern AVPictureTypeNone,
	pattern AVPictureTypeI,
	pattern AVPictureTypeP,
	pattern AVPictureTypeB,
	pattern AVPictureTypeS,
	pattern AVPictureTypeSi,
	pattern AVPictureTypeSp,
	pattern AVPictureTypeBi
) where

import Control.Applicative
import Foreign.Ptr

import Foreign.C.Types
import Foreign.Storable
import Media.FFMpeg.Internal.Common

#include "ffmpeg.h"

-- | AVFrameSideDataType enum
newtype AVFrameSideDataType = AVFrameSideDataType CInt deriving (Eq, Show, CEnum)
pattern AVFrameDataTypePanscan = AVFrameSideDataType (#{const AV_FRAME_DATA_PANSCAN})
pattern AVFrameDataTypeA53CC = AVFrameSideDataType (#{const AV_FRAME_DATA_A53_CC})
pattern AVFrameDataTypeStereo3d = AVFrameSideDataType (#{const AV_FRAME_DATA_STEREO3D})
pattern AVFrameDataTypeMatrixEncoding = AVFrameSideDataType (#{const AV_FRAME_DATA_MATRIXENCODING})
pattern AVFrameDataTypeDownmixInfo = AVFrameSideDataType (#{const AV_FRAME_DATA_DOWNMIX_INFO})
pattern AVFrameDataTypeReplayGain = AVFrameSideDataType (#{const AV_FRAME_DATA_REPLAYGAIN})
pattern AVFrameDataTypeDisplayMatrix = AVFrameSideDataType (#{const AV_FRAME_DATA_DISPLAYMATRIX})
pattern AVFrameDataTypeAfd = AVFrameSideDataType (#{const AV_FRAME_DATA_AFD})
pattern AVFrameDataTypeMotionVectors = AVFrameSideDataType (#{const AV_FRAME_DATA_MOTION_VECTORS})
pattern AVFrameDataTypeSkipSamples = AVFrameSideDataType (#{const AV_FRAME_DATA_SKIP_SAMPLES})

-- | AV_NUM_DATA_POINTERS
pattern AVNumDataPointers = #{const AV_NUM_DATA_POINTERS}

-- | AVActiveFormatDescription
newtype AVActiveFormatDescription = AVActiveFormatDescription CInt deriving (Eq, Show, CEnum)
pattern AVAfdSame = AVActiveFormatDescription (#{const AV_AFD_SAME})
pattern AVAfd4_3 = AVActiveFormatDescription (#{const AV_AFD_4_3})
pattern AVAfd16_9 = AVActiveFormatDescription (#{const AV_AFD_16_9})
pattern AVAfd14_9 = AVActiveFormatDescription (#{const AV_AFD_14_9})
pattern AVAfd4_3_SP_14_9 = AVActiveFormatDescription (#{const AV_AFD_4_3_SP_14_9})
pattern AVAfd16_9_SP_14_9 = AVActiveFormatDescription (#{const AV_AFD_16_9_SP_14_9})
pattern AVAfdSp_4_3 = AVActiveFormatDescription (#{const AV_AFD_SP_4_3})

-- | PixelFormat enumeration
newtype PixelFormat = PixelFormat CInt deriving (Eq, Show, CEnum, Storable)
pattern PixFmtNone = PixelFormat (#{const PIX_FMT_NONE})
pattern PixFmtYuv420p = PixelFormat (#{const PIX_FMT_YUV420P})
pattern PixFmtYuyv422 = PixelFormat (#{const PIX_FMT_YUYV422})
pattern PixFmtRgb24 = PixelFormat (#{const PIX_FMT_RGB24})
pattern PixFmtBgr24 = PixelFormat (#{const PIX_FMT_BGR24})
pattern PixFmtYuv422p = PixelFormat (#{const PIX_FMT_YUV422P})
pattern PixFmtYuv444p = PixelFormat (#{const PIX_FMT_YUV444P})
pattern PixFmtYuv410p = PixelFormat (#{const PIX_FMT_YUV410P})
pattern PixFmtYuv411p = PixelFormat (#{const PIX_FMT_YUV411P})
pattern PixFmtGray8 = PixelFormat (#{const PIX_FMT_GRAY8})
pattern PixFmtMonowhite = PixelFormat (#{const PIX_FMT_MONOWHITE})
pattern PixFmtMonoblack = PixelFormat (#{const PIX_FMT_MONOBLACK})
pattern PixFmtPal8 = PixelFormat (#{const PIX_FMT_PAL8})
pattern PixFmtYuvj420p = PixelFormat (#{const PIX_FMT_YUVJ420P})
pattern PixFmtYuvj422p = PixelFormat (#{const PIX_FMT_YUVJ422P})
pattern PixFmtYuvj444p = PixelFormat (#{const PIX_FMT_YUVJ444P})
pattern PixFmtXvmcMpeg2Mc = PixelFormat (#{const PIX_FMT_XVMC_MPEG2_MC})
pattern PixFmtXvmcMpeg2Idct = PixelFormat (#{const PIX_FMT_XVMC_MPEG2_IDCT})
pattern PixFmtUyvy422 = PixelFormat (#{const PIX_FMT_UYVY422})
pattern PixFmtUyyvyy411 = PixelFormat (#{const PIX_FMT_UYYVYY411})
pattern PixFmtBgr8 = PixelFormat (#{const PIX_FMT_BGR8})
pattern PixFmtBgr4 = PixelFormat (#{const PIX_FMT_BGR4})
pattern PixFmtBgr4Byte = PixelFormat (#{const PIX_FMT_BGR4_BYTE})
pattern PixFmtRgb8 = PixelFormat (#{const PIX_FMT_RGB8})
pattern PixFmtRgb4 = PixelFormat (#{const PIX_FMT_RGB4})
pattern PixFmtRgb4Byte = PixelFormat (#{const PIX_FMT_RGB4_BYTE})
pattern PixFmtNv12 = PixelFormat (#{const PIX_FMT_NV12})
pattern PixFmtNv21 = PixelFormat (#{const PIX_FMT_NV21})
pattern PixFmtArgb = PixelFormat (#{const PIX_FMT_ARGB})
pattern PixFmtRgba = PixelFormat (#{const PIX_FMT_RGBA})
pattern PixFmtAbgr = PixelFormat (#{const PIX_FMT_ABGR})
pattern PixFmtBgra = PixelFormat (#{const PIX_FMT_BGRA})
pattern PixFmtGray16be = PixelFormat (#{const PIX_FMT_GRAY16BE})
pattern PixFmtGray16le = PixelFormat (#{const PIX_FMT_GRAY16LE})
pattern PixFmtRgb32 = PixelFormat (#{const PIX_FMT_RGB32})
pattern PixFmtRgb32_1 = PixelFormat (#{const PIX_FMT_RGB32_1})
pattern PixFmtBgr32 = PixelFormat (#{const PIX_FMT_BGR32})
pattern PixFmtBgr32_1 = PixelFormat (#{const PIX_FMT_BGR32_1})
pattern PixFmtGray16 = PixelFormat (#{const PIX_FMT_GRAY16})
pattern PixFmtRgb565 = PixelFormat (#{const PIX_FMT_RGB565})
pattern PixFmtRgb555 = PixelFormat (#{const PIX_FMT_RGB555})
pattern PixFmtBgr565 = PixelFormat (#{const PIX_FMT_BGR565})
pattern PixFmtBgr555 = PixelFormat (#{const PIX_FMT_BGR555})

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
pattern AVColPriReserved0 = AVColorPrimaries (#{const AVCOL_PRI_RESERVED0})
pattern AVColPriBt709 = AVColorPrimaries (#{const AVCOL_PRI_BT709})
pattern AVColPriUnspecified = AVColorPrimaries (#{const AVCOL_PRI_UNSPECIFIED})
pattern AVColPriReserved = AVColorPrimaries (#{const AVCOL_PRI_RESERVED})
pattern AVColPriBt470m = AVColorPrimaries (#{const AVCOL_PRI_BT470M})
pattern AVColPriBt470bg = AVColorPrimaries (#{const AVCOL_PRI_BT470BG})
pattern AVColPriSmpte170m = AVColorPrimaries (#{const AVCOL_PRI_SMPTE170M})
pattern AVColPriSmpte240m = AVColorPrimaries (#{const AVCOL_PRI_SMPTE240M})
pattern AVColPriFilm = AVColorPrimaries (#{const AVCOL_PRI_FILM})
pattern AVColPriBt2020 = AVColorPrimaries (#{const AVCOL_PRI_BT2020})

-- | AVColorTransferCharacteristic enum
newtype AVColorTransferCharacteristic = AVColorTransferCharacteristic CInt deriving (Eq, Show, CEnum)
pattern AVColTrcReserved0 = AVColorTransferCharacteristic (#{const AVCOL_TRC_RESERVED0})
pattern AVColTrcBt709 = AVColorTransferCharacteristic (#{const AVCOL_TRC_BT709})
pattern AVColTrcUnspecified = AVColorTransferCharacteristic (#{const AVCOL_TRC_UNSPECIFIED})
pattern AVColTrcReserved = AVColorTransferCharacteristic (#{const AVCOL_TRC_RESERVED})
pattern AVColTrcGamma22 = AVColorTransferCharacteristic (#{const AVCOL_TRC_GAMMA22})
pattern AVColTrcGamma28 = AVColorTransferCharacteristic (#{const AVCOL_TRC_GAMMA28})
pattern AVColTrcSmpte170m = AVColorTransferCharacteristic (#{const AVCOL_TRC_SMPTE170M})
pattern AVColTrcSmpte240m = AVColorTransferCharacteristic (#{const AVCOL_TRC_SMPTE240M})
pattern AVColTrcLinear = AVColorTransferCharacteristic (#{const AVCOL_TRC_LINEAR})
pattern AVColTrcLog = AVColorTransferCharacteristic (#{const AVCOL_TRC_LOG})
pattern AVColTrcLog_sqrt = AVColorTransferCharacteristic (#{const AVCOL_TRC_LOG_SQRT})
pattern AVColTrcIec61966_2_4 = AVColorTransferCharacteristic (#{const AVCOL_TRC_IEC61966_2_4})
pattern AVColTrcBt1361Ecg = AVColorTransferCharacteristic (#{const AVCOL_TRC_BT1361_ECG})
pattern AVColTrcIec61966_2_1 = AVColorTransferCharacteristic (#{const AVCOL_TRC_IEC61966_2_1})
pattern AVColTrcBt2020_10 = AVColorTransferCharacteristic (#{const AVCOL_TRC_BT2020_10})
pattern AVColTrcBt2020_12 = AVColorTransferCharacteristic (#{const AVCOL_TRC_BT2020_12})

-- | AVColorSpace enum
newtype AVColorSpace = AVColorSpace CInt deriving (Eq, Show, CEnum)
pattern AVColSpcRgb = AVColorSpace (#{const AVCOL_SPC_RGB})
pattern AVColSpcBt709 = AVColorSpace (#{const AVCOL_SPC_BT709})
pattern AVColSpcUnspecified = AVColorSpace (#{const AVCOL_SPC_UNSPECIFIED})
pattern AVColSpcReserved = AVColorSpace (#{const AVCOL_SPC_RESERVED})
pattern AVColSpcFcc = AVColorSpace (#{const AVCOL_SPC_FCC})
pattern AVColSpcBt470bg = AVColorSpace (#{const AVCOL_SPC_BT470BG})
pattern AVColSpcSmpte170m = AVColorSpace (#{const AVCOL_SPC_SMPTE170M})
pattern AVColSpcSmpte240m = AVColorSpace (#{const AVCOL_SPC_SMPTE240M})
pattern AVColSpcYcocg = AVColorSpace (#{const AVCOL_SPC_YCOCG})
pattern AVColSpcBt2020_ncl = AVColorSpace (#{const AVCOL_SPC_BT2020_NCL})
pattern AVColSpcBt2020_cl = AVColorSpace (#{const AVCOL_SPC_BT2020_CL})
pattern AVColSpcYcgco = AVColorSpace (#{const AVCOL_SPC_YCGCO})

-- | AVColorRange enum
newtype AVColorRange = AVColorRange CInt deriving (Eq, Show, CEnum)
pattern AVColRangeUnspecified = AVColorRange (#{const AVCOL_RANGE_UNSPECIFIED})
pattern AVColRangeMpeg = AVColorSpace (#{const AVCOL_RANGE_MPEG})
pattern AVColRangeJpeg = AVColorSpace (#{const AVCOL_RANGE_JPEG})

-- | AVChromaLocation enum
newtype AVChromaLocation = AVChromaLocation CInt deriving (Eq, Show, CEnum)
pattern AVChromaLocUnspecified = AVChromaLocation (#{const AVCHROMA_LOC_UNSPECIFIED})
pattern AVChromaLocLeft = AVChromaLocation (#{const AVCHROMA_LOC_LEFT})
pattern AVChromaLocCenter = AVChromaLocation (#{const AVCHROMA_LOC_CENTER})
pattern AVChromaLocTopleft = AVChromaLocation (#{const AVCHROMA_LOC_TOPLEFT})
pattern AVChromaLocTop = AVChromaLocation (#{const AVCHROMA_LOC_TOP})
pattern AVChromaLocBottomleft = AVChromaLocation (#{const AVCHROMA_LOC_BOTTOMLEFT})
pattern AVChromaLocBottom = AVChromaLocation (#{const AVCHROMA_LOC_BOTTOM})

-- | AVStereo3DType enum
newtype AVStereo3DType = AVStereo3DType CInt deriving (Eq, Show, CEnum)
pattern AVStereo3d2d = AVStereo3DType (#{const AV_STEREO3D_2D})
pattern AVStereo3dSidebyside = AVStereo3DType (#{const AV_STEREO3D_SIDEBYSIDE})
pattern AVStereo3dTopbottom = AVStereo3DType (#{const AV_STEREO3D_TOPBOTTOM})
pattern AVStereo3dFramesequence = AVStereo3DType (#{const AV_STEREO3D_FRAMESEQUENCE})
pattern AVStereo3dCheckerboard = AVStereo3DType (#{const AV_STEREO3D_CHECKERBOARD})
pattern AVStereo3dSidebysideQuincunx = AVStereo3DType (#{const AV_STEREO3D_SIDEBYSIDE_QUINCUNX})
pattern AVStereo3dLines = AVStereo3DType (#{const AV_STEREO3D_LINES})
pattern AVStereo3dColumns = AVStereo3DType (#{const AV_STEREO3D_COLUMNS})

-- | Flags for the AVStereo3D struct
newtype AVStereo3DFlags = AVStereo3DFlags CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVStereo3dFlagInvert = AVStereo3DFlags (#{const AV_STEREO3D_FLAG_INVERT})

-- | AVDownmixType enum
newtype AVDownmixType = AVDownmixType CInt deriving (Eq, Show, CEnum)
pattern AVDownmixTypeUnknown = AVDownmixType (#{const AV_DOWNMIX_TYPE_UNKNOWN})
pattern AVDownmixTypeLoro = AVDownmixType (#{const AV_DOWNMIX_TYPE_LORO})
pattern AVDownmixTypeLtrt = AVDownmixType (#{const AV_DOWNMIX_TYPE_LTRT})
pattern AVDownmixTypeDplii = AVDownmixType (#{const AV_DOWNMIX_TYPE_DPLII})

-- | AVOptType enum
newtype AVOptType = AVOptType CInt deriving (Eq, CEnum, Storable)
pattern AVOptTypeFlags = AVOptType #{const AV_OPT_TYPE_FLAGS}
pattern AVOptTypeInt = AVOptType #{const AV_OPT_TYPE_INT}
pattern AVOptTypeInt64 = AVOptType #{const AV_OPT_TYPE_INT64}
pattern AVOptTypeDouble = AVOptType #{const AV_OPT_TYPE_DOUBLE}
pattern AVOptTypeFloat = AVOptType #{const AV_OPT_TYPE_FLOAT}
pattern AVOptTypeString = AVOptType #{const AV_OPT_TYPE_STRING}
pattern AVOptTypeRational = AVOptType #{const AV_OPT_TYPE_RATIONAL}
pattern AVOptTypeBinary = AVOptType #{const AV_OPT_TYPE_BINARY}
pattern AVOptTypeDict = AVOptType #{const AV_OPT_TYPE_DICT}
pattern AVOptTypeConst = AVOptType #{const AV_OPT_TYPE_CONST}
pattern AVOptTypeImageSize = AVOptType #{const AV_OPT_TYPE_IMAGE_SIZE}
pattern AVOptTypePixelFmt = AVOptType #{const AV_OPT_TYPE_PIXEL_FMT}
pattern AVOptTypeSampleFmt = AVOptType #{const AV_OPT_TYPE_SAMPLE_FMT}
pattern AVOptTypeVideoRate = AVOptType #{const AV_OPT_TYPE_VIDEO_RATE}
pattern AVOptTypeDuration = AVOptType #{const AV_OPT_TYPE_DURATION}
pattern AVOptTypeColor = AVOptType #{const AV_OPT_TYPE_COLOR}
pattern AVOptTypeChannelLayout = AVOptType #{const AV_OPT_TYPE_CHANNEL_LAYOUT}

instance Show AVOptType where
	show x = case x of
		AVOptTypeFlags -> "AVOptTypeFlags"
		AVOptTypeInt -> "AVOptTypeInt"
		AVOptTypeInt64 -> "AVOptTypeInt64"
		AVOptTypeDouble -> "AVOptTypeDouble"
		AVOptTypeFloat -> "AVOptTypeFloat"
		AVOptTypeString -> "AVOptTypeString"
		AVOptTypeRational -> "AVOptTypeRational"
		AVOptTypeBinary -> "AVOptTypeBinary"
		AVOptTypeDict -> "AVOptTypeDict"
		AVOptTypeConst -> "AVOptTypeConst"
		AVOptTypeImageSize -> "AVOptTypeImageSize"
		AVOptTypePixelFmt -> "AVOptTypePixelFmt"
		AVOptTypeSampleFmt -> "AVOptTypeSampleFmt"
		AVOptTypeVideoRate -> "AVOptTypeVideoRate"
		AVOptTypeDuration -> "AVOptTypeDuration"
		AVOptTypeColor -> "AVOptTypeColor"
		AVOptTypeChannelLayout -> "AVOptTypeChannelLayout"
		_ -> "AVOptTypeUnknown"

-- | AVOptionFlags enum
newtype AVOptionFlags = AVOptionFlags CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVOptFlagEncodingParam = AVOptionFlags (#{const AV_OPT_FLAG_ENCODING_PARAM})
pattern AVOptFlagDecodingParam = AVOptionFlags (#{const AV_OPT_FLAG_DECODING_PARAM})
pattern AVOptFlagAudioParam = AVOptionFlags (#{const AV_OPT_FLAG_AUDIO_PARAM})
pattern AVOptFlagVideoParam = AVOptionFlags (#{const AV_OPT_FLAG_VIDEO_PARAM})
pattern AVOptFlagSubtitleParam = AVOptionFlags (#{const AV_OPT_FLAG_SUBTITLE_PARAM})
pattern AVOptFlagExport = AVOptionFlags (#{const AV_OPT_FLAG_EXPORT})
pattern AVOptFlagReadonly = AVOptionFlags (#{const AV_OPT_FLAG_READONLY})
pattern AVOptFlagFilteringParam = AVOptionFlags (#{const AV_OPT_FLAG_FILTERING_PARAM})

-- | AV_OPT_SEARCH_ flags
newtype AVOptionSearchFlags = AVOptionSearchFlags CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVOptSearchChildren = AVOptionSearchFlags (#{const AV_OPT_SEARCH_CHILDREN})
pattern AVOptSearchFakeObj = AVOptionSearchFlags (#{const AV_OPT_SEARCH_FAKE_OBJ})

-- | AVPictureType enum
newtype AVPictureType = AVPictureType CInt deriving (Eq, Show, CEnum, Storable)
pattern AVPictureTypeNone = AVPictureType (#{const AV_PICTURE_TYPE_NONE})
pattern AVPictureTypeI = AVPictureType (#{const AV_PICTURE_TYPE_I})
pattern AVPictureTypeP = AVPictureType (#{const AV_PICTURE_TYPE_P})
pattern AVPictureTypeB = AVPictureType (#{const AV_PICTURE_TYPE_B})
pattern AVPictureTypeS = AVPictureType (#{const AV_PICTURE_TYPE_S})
pattern AVPictureTypeSi = AVPictureType (#{const AV_PICTURE_TYPE_SI})
pattern AVPictureTypeSp = AVPictureType (#{const AV_PICTURE_TYPE_SP})
pattern AVPictureTypeBi = AVPictureType (#{const AV_PICTURE_TYPE_BI})

