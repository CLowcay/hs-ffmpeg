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
	AVFmt,
	pattern AVFmtNoFile,
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

	AVFmtFlag,
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

	AVFmtAvoidNegTS,
	pattern AVFmtAvoidNegTsAuto,
	pattern AVFmtAvoidNegTsMakeNonNegative,
	pattern AVFmtAvoidNegTsMakeZero,

	AVSeekFlag,
	pattern AVSeekFlagBackward,
	pattern AVSeekFlagByte,
	pattern AVSeekFlagAny,
	pattern AVSeekFlagFrame,

	AVIOFlag,
	pattern AVIOFlagNonblock,
	pattern AVIOFlagDirect,

	FFFDebug,
	pattern FFFDebugTs,

	AVStreamEventFlag,
	pattern AVStreamEventFlagMetadataUpdated,

	AVFmtEventFlag,
	pattern AVFmtEventFlagMetadataUpdated,

	AVStreamParseType,
	pattern AVStreamParseNone,
	pattern AVStreamParseFull,
	pattern AVStreamParseHeaders,
	pattern AVStreamParseTimestamps,
	pattern AVStreamParseFullOnce,
	pattern AVStreamParseFullRaw,

	AVDurationEstimationMethod,
	pattern AVFmtDurationFromPts,
	pattern AVFmtDurationFromStream,
	pattern AVFmtDurationFromBitrate,

	AVFmtCtx,
	pattern AVFmtCtxNoheader,

	pattern AVIOSeekableNormal,

	AVIOSeek,
	pattern AVSeekSize,
	pattern AVSeekForce,

	AVIOOpenFlag,
	pattern AVIOFlagRead,
	pattern AVIOFlagWrite,
	pattern AVIOFlagReadWrite
) where

#include "ffmpeg.h"

import Foreign.C.Types
import Foreign.Storable

import Media.FFMpeg.Internal.Common

-- | AVFMT_ flags
newtype AVFmt = AVFmt CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVFmtNoFile = AVFmt (#{const AVFMT_NOFILE})
pattern AVFmtNeednumber = AVFmt (#{const AVFMT_NEEDNUMBER})
pattern AVFmtShowIds = AVFmt (#{const AVFMT_SHOW_IDS})
pattern AVFmtRawpicture = AVFmt (#{const AVFMT_RAWPICTURE})
pattern AVFmtGlobalheader = AVFmt (#{const AVFMT_GLOBALHEADER})
pattern AVFmtNotimestamps = AVFmt (#{const AVFMT_NOTIMESTAMPS})
pattern AVFmtGenericIndex = AVFmt (#{const AVFMT_GENERIC_INDEX})
pattern AVFmtTsDiscont = AVFmt (#{const AVFMT_TS_DISCONT})
pattern AVFmtVariableFps = AVFmt (#{const AVFMT_VARIABLE_FPS})
pattern AVFmtNodimensions = AVFmt (#{const AVFMT_NODIMENSIONS})
pattern AVFmtNostreams = AVFmt (#{const AVFMT_NOSTREAMS})
pattern AVFmtNobinsearch = AVFmt (#{const AVFMT_NOBINSEARCH})
pattern AVFmtNogensearch = AVFmt (#{const AVFMT_NOGENSEARCH})
pattern AVFmtNoByteSeek = AVFmt (#{const AVFMT_NO_BYTE_SEEK})
pattern AVFmtAllowFlush = AVFmt (#{const AVFMT_ALLOW_FLUSH})
pattern AVFmtTsNonstrict = AVFmt (#{const AVFMT_TS_NONSTRICT})
pattern AVFmtTsNegative = AVFmt (#{const AVFMT_TS_NEGATIVE})
pattern AVFmtSeekToPts = AVFmt (#{const AVFMT_SEEK_TO_PTS})

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
newtype AVFmtFlag = AVFMTFlag CInt deriving (Eq, Show, CEnum, CFlags)
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
newtype AVFmtAvoidNegTS = AVFMTAvoidNegTS CInt deriving (Eq, Show, CEnum)
pattern AVFmtAvoidNegTsAuto = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_AUTO})
pattern AVFmtAvoidNegTsMakeNonNegative = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_MAKE_NON_NEGATIVE})
pattern AVFmtAvoidNegTsMakeZero = AVFMTAvoidNegTS (#{const AVFMT_AVOID_NEG_TS_MAKE_ZERO})

-- | AVSEEK_FLAG_ flags
newtype AVSeekFlag = AVSeekFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVSeekFlagBackward = AVSeekFlag (#{const AVSEEK_FLAG_BACKWARD})
pattern AVSeekFlagByte = AVSeekFlag (#{const AVSEEK_FLAG_BYTE})
pattern AVSeekFlagAny = AVSeekFlag (#{const AVSEEK_FLAG_ANY})
pattern AVSeekFlagFrame = AVSeekFlag (#{const AVSEEK_FLAG_FRAME})

-- | AVIO_FLAG_ flags
newtype AVIOFlag = AVIOFlag CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVIOFlagNonblock = AVIOFlag (#{const AVIO_FLAG_NONBLOCK})
pattern AVIOFlagDirect = AVIOFlag (#{const AVIO_FLAG_DIRECT})

-- | FF_FDEBUG_ flags
newtype FFFDebug = FFFDebug CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern FFFDebugTs = FFFDebug (#{const FF_FDEBUG_TS}) 

-- | AVSTREAM_EVENT_FLAG_ flags
newtype AVStreamEventFlag = AVStreamEventFlag CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVStreamEventFlagMetadataUpdated = AVStreamEventFlag (#{const AVSTREAM_EVENT_FLAG_METADATA_UPDATED})

-- | AVFMT_EVENT_FLAG_ flags
newtype AVFmtEventFlag = AVFmtEventFlag CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVFmtEventFlagMetadataUpdated = AVFmtEventFlag (#{const AVFMT_EVENT_FLAG_METADATA_UPDATED})

-- | AVStreamParseType enum
newtype AVStreamParseType = AVStreamParseType CInt deriving (Eq, Show, CEnum, Storable)
pattern AVStreamParseNone = AVStreamParseType (#{const AVSTREAM_PARSE_NONE})
pattern AVStreamParseFull = AVStreamParseType (#{const AVSTREAM_PARSE_FULL})
pattern AVStreamParseHeaders = AVStreamParseType (#{const AVSTREAM_PARSE_HEADERS})
pattern AVStreamParseTimestamps = AVStreamParseType (#{const AVSTREAM_PARSE_TIMESTAMPS})
pattern AVStreamParseFullOnce = AVStreamParseType (#{const AVSTREAM_PARSE_FULL_ONCE})
pattern AVStreamParseFullRaw = AVStreamParseType (#{const AVSTREAM_PARSE_FULL_RAW})

-- | AVDurationEstimationMethod enum
newtype AVDurationEstimationMethod = AVDurationEstimationMethod CInt deriving (Eq, Show, CEnum, Storable)
pattern AVFmtDurationFromPts = AVDurationEstimationMethod (#{const AVFMT_DURATION_FROM_PTS})
pattern AVFmtDurationFromStream = AVDurationEstimationMethod (#{const AVFMT_DURATION_FROM_STREAM})
pattern AVFmtDurationFromBitrate = AVDurationEstimationMethod (#{const AVFMT_DURATION_FROM_BITRATE})

-- | AVFMTCTX_ flags
newtype AVFmtCtx = AVFmtCtx CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVFmtCtxNoheader = AVFmtCtx (#{const AVFMTCTX_NOHEADER})

-- | AVIO_SEEKABLE_NORMAL constant
pattern AVIOSeekableNormal = #{const AVIO_SEEKABLE_NORMAL}

-- | AVSEEK_ constants
newtype AVIOSeek = AVIOSeek CInt deriving (Eq, Show, CEnum, Storable)
pattern AVSeekSize = AVIOSeek (#{const AVSEEK_SIZE})
pattern AVSeekForce = AVIOSeek (#{const AVSEEK_FORCE})

-- | AVIO_FLAG_ flags for avio_open
newtype AVIOOpenFlag = AVIOOpenFlag CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVIOFlagRead = AVIOOpenFlag (#{const AVIO_FLAG_READ})
pattern AVIOFlagWrite = AVIOOpenFlag (#{const AVIO_FLAG_WRITE})
pattern AVIOFlagReadWrite = AVIOOpenFlag (#{const AVIO_FLAG_READ_WRITE})

-- | AVIODirEntryType enum
-- newtype AVIODirEntryType = AVIODirEntryType CInt deriving (Eq, Show, CEnum, Storable)
-- pattern AVIOEntryUnknown = AVIODirEntryType (#{const AVIO_ENTRY_UNKNOWN})
-- pattern AVIOEntryBlockDevice = AVIODirEntryType (#{const AVIO_ENTRY_BLOCK_DEVICE})
-- pattern AVIOEntryCharacterDevice = AVIODirEntryType (#{const AVIO_ENTRY_CHARACTER_DEVICE})
-- pattern AVIOEntryDirectory = AVIODirEntryType (#{const AVIO_ENTRY_DIRECTORY})
-- pattern AVIOEntryNamedPipe = AVIODirEntryType (#{const AVIO_ENTRY_NAMED_PIPE})
-- pattern AVIOEntrySymbolicLink = AVIODirEntryType (#{const AVIO_ENTRY_SYMBOLIC_LINK})
-- pattern AVIOEntrySocket = AVIODirEntryType (#{const AVIO_ENTRY_SOCKET})
-- pattern AVIOEntryFile = AVIODirEntryType (#{const AVIO_ENTRY_FILE})
-- pattern AVIOEntryServer = AVIODirEntryType (#{const AVIO_ENTRY_SERVER})
-- pattern AVIOEntryShare = AVIODirEntryType (#{const AVIO_ENTRY_SHARE})
-- pattern AVIOEntryWorkgroup = AVIODirEntryType (#{const AVIO_ENTRY_WORKGROUP})

