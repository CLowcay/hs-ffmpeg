{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Enums (
	AVFmtFlag,
	pattern AVFmtNeednumber,
	pattern AVFmtShowIds,
	pattern AVFmtRawpicture,
	pattern AVFmtGlobalheader,
	pattern AVFmtNotimestamps,
	pattern AVFmtGenericIndex,
	pattern AVFmtTsDiscont,
	pattern AVFmtVariableFps,
	pattern AVFmtNodimensions,
	pattern AVFmtNostreams,
	pattern AVFmtNobinsearch,
	pattern AVFmtNogensearch,
	pattern AVFmtNoByteSeek,
	pattern AVFmtAllowFlush,
	pattern AVFmtTsNonstrict,
	pattern AVFmtTsNegative,
	pattern AVFmtSeekToPts,

	AVDispositionFlag,
	pattern AVDispositionDefault,
	pattern AVDispositionDub,
	pattern AVDispositionOriginal,
	pattern AVDispositionComment,
	pattern AVDispositionLyrics,
	pattern AVDispositionKaraoke,
	pattern AVDispositionForced,
	pattern AVDispositionHearingImpaired,
	pattern AVDispositionVisualImpaired,
	pattern AVDispositionCleanEffects,
	pattern AVDispositionAttachedPic,
	pattern AVDispositionCaptions,
	pattern AVDispositionDescriptions,
	pattern AVDispositionMetadata,

	AVPTSWrap,
	pattern AVPtsWrapIgnore,
	pattern AVPtsWrapAddOffset,
	pattern AVPtsWrapSubOffset,

	AVFMTFlag,
	pattern AVFmtFlagGenpts,
	pattern AVFmtFlagIgnidx,
	pattern AVFmtFlagNonblock,
	pattern AVFmtFlagIgndts,
	pattern AVFmtFlagNofillin,
	pattern AVFmtFlagNoparse,
	pattern AVFmtFlagNobuffer,
	pattern AVFmtFlagCustomIo,
	pattern AVFmtFlagDiscardCorrupt,
	pattern AVFmtFlagFlushPackets,
	pattern AVFmtFlagBitexact,
	pattern AVFmtFlagMp4aLatm,
	pattern AVFmtFlagSortDts,
	pattern AVFmtFlagPrivOpt,
	pattern AVFmtFlagKeepSideData,

	AVFMTAvoidNegTS,
	pattern AVFmtAvoidNegTsAuto,
	pattern AVFmtAvoidNegTsMakeNonNegative,
	pattern AVFmtAvoidNegTsMakeZero,

	AVSeekFlag,
	pattern AVSeekFlagBackward,
	pattern AVSeekFlagByte,
	pattern AVSeekFlagAny,
	pattern AVSeekFlagFrame
) where

#include "ffmpeg.h"

import Foreign.C.Types

import Media.FFMpeg.Internal.Common

-- | AVFMT_ flags
newtype AVFmtFlag = AVFmtFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVFmtNeednumber = AVFmtFlag (#{const AVFMT_NEEDNUMBER})
pattern AVFmtShowIds = AVFmtFlag (#{const AVFMT_SHOW_IDS})
pattern AVFmtRawpicture = AVFmtFlag (#{const AVFMT_RAWPICTURE})
pattern AVFmtGlobalheader = AVFmtFlag (#{const AVFMT_GLOBALHEADER})
pattern AVFmtNotimestamps = AVFmtFlag (#{const AVFMT_NOTIMESTAMPS})
pattern AVFmtGenericIndex = AVFmtFlag (#{const AVFMT_GENERIC_INDEX})
pattern AVFmtTsDiscont = AVFmtFlag (#{const AVFMT_TS_DISCONT})
pattern AVFmtVariableFps = AVFmtFlag (#{const AVFMT_VARIABLE_FPS})
pattern AVFmtNodimensions = AVFmtFlag (#{const AVFMT_NODIMENSIONS})
pattern AVFmtNostreams = AVFmtFlag (#{const AVFMT_NOSTREAMS})
pattern AVFmtNobinsearch = AVFmtFlag (#{const AVFMT_NOBINSEARCH})
pattern AVFmtNogensearch = AVFmtFlag (#{const AVFMT_NOGENSEARCH})
pattern AVFmtNoByteSeek = AVFmtFlag (#{const AVFMT_NO_BYTE_SEEK})
pattern AVFmtAllowFlush = AVFmtFlag (#{const AVFMT_ALLOW_FLUSH})
pattern AVFmtTsNonstrict = AVFmtFlag (#{const AVFMT_TS_NONSTRICT})
pattern AVFmtTsNegative = AVFmtFlag (#{const AVFMT_TS_NEGATIVE})
pattern AVFmtSeekToPts = AVFmtFlag (#{const AVFMT_SEEK_TO_PTS})

-- | AV_DISPOSITION_ flags
newtype AVDispositionFlag = AVDispositionFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVDispositionDefault = AVDispositionFlag (#{const AV_DISPOSITION_DEFAULT})
pattern AVDispositionDub = AVDispositionFlag (#{const AV_DISPOSITION_DUB})
pattern AVDispositionOriginal = AVDispositionFlag (#{const AV_DISPOSITION_ORIGINAL})
pattern AVDispositionComment = AVDispositionFlag (#{const AV_DISPOSITION_COMMENT})
pattern AVDispositionLyrics = AVDispositionFlag (#{const AV_DISPOSITION_LYRICS})
pattern AVDispositionKaraoke = AVDispositionFlag (#{const AV_DISPOSITION_KARAOKE})
pattern AVDispositionForced = AVDispositionFlag (#{const AV_DISPOSITION_FORCED})
pattern AVDispositionHearingImpaired = AVDispositionFlag (#{const AV_DISPOSITION_HEARING_IMPAIRED})
pattern AVDispositionVisualImpaired = AVDispositionFlag (#{const AV_DISPOSITION_VISUAL_IMPAIRED})
pattern AVDispositionCleanEffects = AVDispositionFlag (#{const AV_DISPOSITION_CLEAN_EFFECTS})
pattern AVDispositionAttachedPic = AVDispositionFlag (#{const AV_DISPOSITION_ATTACHED_PIC})
pattern AVDispositionCaptions = AVDispositionFlag (#{const AV_DISPOSITION_CAPTIONS})
pattern AVDispositionDescriptions = AVDispositionFlag (#{const AV_DISPOSITION_DESCRIPTIONS})
pattern AVDispositionMetadata = AVDispositionFlag (#{const AV_DISPOSITION_METADATA})

-- | AV_PTS_WRAP_ constants
newtype AVPTSWrap = AVPTSWrap CInt deriving (Eq, Show, CEnum)
pattern AVPtsWrapIgnore = AVPTSWrap (#{const AV_PTS_WRAP_IGNORE})
pattern AVPtsWrapAddOffset = AVPTSWrap (#{const AV_PTS_WRAP_ADD_OFFSET})
pattern AVPtsWrapSubOffset = AVPTSWrap (#{const AV_PTS_WRAP_SUB_OFFSET})

-- | AVFMT_FLAG_ flags
newtype AVFMTFlag = AVFMTFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVFmtFlagGenpts = AVFMTFlag (#{const AVFMT_FLAG_GENPTS})
pattern AVFmtFlagIgnidx = AVFMTFlag (#{const AVFMT_FLAG_IGNIDX})
pattern AVFmtFlagNonblock = AVFMTFlag (#{const AVFMT_FLAG_NONBLOCK})
pattern AVFmtFlagIgndts = AVFMTFlag (#{const AVFMT_FLAG_IGNDTS})
pattern AVFmtFlagNofillin = AVFMTFlag (#{const AVFMT_FLAG_NOFILLIN})
pattern AVFmtFlagNoparse = AVFMTFlag (#{const AVFMT_FLAG_NOPARSE})
pattern AVFmtFlagNobuffer = AVFMTFlag (#{const AVFMT_FLAG_NOBUFFER})
pattern AVFmtFlagCustomIo = AVFMTFlag (#{const AVFMT_FLAG_CUSTOM_IO})
pattern AVFmtFlagDiscardCorrupt = AVFMTFlag (#{const AVFMT_FLAG_DISCARD_CORRUPT})
pattern AVFmtFlagFlushPackets = AVFMTFlag (#{const AVFMT_FLAG_FLUSH_PACKETS})
pattern AVFmtFlagBitexact = AVFMTFlag (#{const AVFMT_FLAG_BITEXACT})
pattern AVFmtFlagMp4aLatm = AVFMTFlag (#{const AVFMT_FLAG_MP4A_LATM})
pattern AVFmtFlagSortDts = AVFMTFlag (#{const AVFMT_FLAG_SORT_DTS})
pattern AVFmtFlagPrivOpt = AVFMTFlag (#{const AVFMT_FLAG_PRIV_OPT})
pattern AVFmtFlagKeepSideData = AVFMTFlag (#{const AVFMT_FLAG_KEEP_SIDE_DATA})

-- avfmt_flag_fast_seek = AVFMT_FLAG_FAST_SEEK

-- | AVFMT_AVOID_NEG_TS_ constants
newtype AVFMTAvoidNegTS = AVFMTAvoidNegTS CInt deriving (Eq, Show, CEnum)
pattern AVFmtAvoidNegTsAuto = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_AUTO})
pattern AVFmtAvoidNegTsMakeNonNegative = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_MAKE_NON_NEGATIVE})
pattern AVFmtAvoidNegTsMakeZero = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_MAKE_ZERO})

-- | AVSEEK_FLAG_ flags
newtype AVSeekFlag = AVSeekFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVSeekFlagBackward = AVSeekFlag (#{const AVSEEK_FLAG_BACKWARD})
pattern AVSeekFlagByte = AVSeekFlag (#{const AVSEEK_FLAG_BYTE})
pattern AVSeekFlagAny = AVSeekFlag (#{const AVSEEK_FLAG_ANY})
pattern AVSeekFlagFrame = AVSeekFlag (#{const AVSEEK_FLAG_FRAME})

