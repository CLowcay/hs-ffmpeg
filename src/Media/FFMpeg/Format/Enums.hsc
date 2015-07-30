{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Enums (
	AVFmtFlag,
	avfmt_nofile,
	avfmt_neednumber,
	avfmt_show_ids,
	avfmt_rawpicture,
	avfmt_globalheader,
	avfmt_notimestamps,
	avfmt_generic_index,
	avfmt_ts_discont,
	avfmt_variable_fps,
	avfmt_nodimensions,
	avfmt_nostreams,
	avfmt_nobinsearch,
	avfmt_nogensearch,
	avfmt_no_byte_seek,
	avfmt_allow_flush,
	avfmt_ts_nonstrict,
	avfmt_ts_negative,
	avfmt_seek_to_pts,

	AVDispositionFlag,
	av_disposition_default,
	av_disposition_dub,
	av_disposition_original,
	av_disposition_comment,
	av_disposition_lyrics,
	av_disposition_karaoke,
	av_disposition_forced,
	av_disposition_hearing_impaired,
	av_disposition_visual_impaired,
	av_disposition_clean_effects,
	av_disposition_attached_pic,
	av_disposition_captions,
	av_disposition_descriptions,
	av_disposition_metadata,

	AVPTSWrap,
	av_pts_wrap_ignore,
	av_pts_wrap_add_offset,
	av_pts_wrap_sub_offset,

	AVFMTFlag,
	avfmt_flag_genpts,
	avfmt_flag_ignidx,
	avfmt_flag_nonblock,
	avfmt_flag_igndts,
	avfmt_flag_nofillin,
	avfmt_flag_noparse,
	avfmt_flag_nobuffer,
	avfmt_flag_custom_io,
	avfmt_flag_discard_corrupt,
	avfmt_flag_flush_packets,
	avfmt_flag_bitexact,
	avfmt_flag_mp4a_latm,
	avfmt_flag_sort_dts,
	avfmt_flag_priv_opt,
	avfmt_flag_keep_side_data,

	AVFMTAvoidNegTS,
	avfmt_avoid_neg_ts_auto,
	avfmt_avoid_neg_ts_make_non_negative,
	avfmt_avoid_neg_ts_make_zero,

	AVSeekFlag,
	avseek_flag_backward,
	avseek_flag_byte,
	avseek_flag_any,
	avseek_flag_frame
) where

#include "ffmpeg.h"

import Foreign.C.Types

import Media.FFMpeg.Internal.Common

-- | AVFMT_ flags
newtype AVFmtFlag = AVFmtFlag CInt deriving (Eq, Show, CEnum, CFlags)
#{enum AVFmtFlag, AVFmtFlag,
	avfmt_nofile = AVFMT_NOFILE,
	avfmt_neednumber = AVFMT_NEEDNUMBER,
	avfmt_show_ids = AVFMT_SHOW_IDS,
	avfmt_rawpicture = AVFMT_RAWPICTURE,
	avfmt_globalheader = AVFMT_GLOBALHEADER,
	avfmt_notimestamps = AVFMT_NOTIMESTAMPS,
	avfmt_generic_index = AVFMT_GENERIC_INDEX,
	avfmt_ts_discont = AVFMT_TS_DISCONT,
	avfmt_variable_fps = AVFMT_VARIABLE_FPS,
	avfmt_nodimensions = AVFMT_NODIMENSIONS,
	avfmt_nostreams = AVFMT_NOSTREAMS,
	avfmt_nobinsearch = AVFMT_NOBINSEARCH,
	avfmt_nogensearch = AVFMT_NOGENSEARCH,
	avfmt_no_byte_seek = AVFMT_NO_BYTE_SEEK,
	avfmt_allow_flush = AVFMT_ALLOW_FLUSH,
	avfmt_ts_nonstrict = AVFMT_TS_NONSTRICT,
	avfmt_ts_negative = AVFMT_TS_NEGATIVE,
	avfmt_seek_to_pts = AVFMT_SEEK_TO_PTS
}

-- | AV_DISPOSITION_ flags
newtype AVDispositionFlag = AVDispositionFlag CInt deriving (Eq, Show, CEnum, CFlags)
#{enum AVDispositionFlag, AVDispositionFlag,
	av_disposition_default = AV_DISPOSITION_DEFAULT,
	av_disposition_dub = AV_DISPOSITION_DUB,
	av_disposition_original = AV_DISPOSITION_ORIGINAL,
	av_disposition_comment = AV_DISPOSITION_COMMENT,
	av_disposition_lyrics = AV_DISPOSITION_LYRICS,
	av_disposition_karaoke = AV_DISPOSITION_KARAOKE,
	av_disposition_forced = AV_DISPOSITION_FORCED,
	av_disposition_hearing_impaired = AV_DISPOSITION_HEARING_IMPAIRED,
	av_disposition_visual_impaired = AV_DISPOSITION_VISUAL_IMPAIRED,
	av_disposition_clean_effects = AV_DISPOSITION_CLEAN_EFFECTS,
	av_disposition_attached_pic = AV_DISPOSITION_ATTACHED_PIC,
	av_disposition_captions = AV_DISPOSITION_CAPTIONS,
	av_disposition_descriptions = AV_DISPOSITION_DESCRIPTIONS,
	av_disposition_metadata = AV_DISPOSITION_METADATA
}

-- | AV_PTS_WRAP_ constants
newtype AVPTSWrap = AVPTSWrap CInt deriving (Eq, Show, CEnum)
#{enum AVPTSWrap, AVPTSWrap,
	av_pts_wrap_ignore = AV_PTS_WRAP_IGNORE,
	av_pts_wrap_add_offset = AV_PTS_WRAP_ADD_OFFSET,
	av_pts_wrap_sub_offset = AV_PTS_WRAP_SUB_OFFSET
}

-- | AVFMT_FLAG_ flags
newtype AVFMTFlag = AVFMTFlag CInt deriving (Eq, Show, CEnum, CFlags)
#{enum AVFMTFlag, AVFMTFlag,
	avfmt_flag_genpts = AVFMT_FLAG_GENPTS,
	avfmt_flag_ignidx = AVFMT_FLAG_IGNIDX,
	avfmt_flag_nonblock = AVFMT_FLAG_NONBLOCK,
	avfmt_flag_igndts = AVFMT_FLAG_IGNDTS,
	avfmt_flag_nofillin = AVFMT_FLAG_NOFILLIN,
	avfmt_flag_noparse = AVFMT_FLAG_NOPARSE,
	avfmt_flag_nobuffer = AVFMT_FLAG_NOBUFFER,
	avfmt_flag_custom_io = AVFMT_FLAG_CUSTOM_IO,
	avfmt_flag_discard_corrupt = AVFMT_FLAG_DISCARD_CORRUPT,
	avfmt_flag_flush_packets = AVFMT_FLAG_FLUSH_PACKETS,
	avfmt_flag_bitexact = AVFMT_FLAG_BITEXACT,
	avfmt_flag_mp4a_latm = AVFMT_FLAG_MP4A_LATM,
	avfmt_flag_sort_dts = AVFMT_FLAG_SORT_DTS,
	avfmt_flag_priv_opt = AVFMT_FLAG_PRIV_OPT,
	avfmt_flag_keep_side_data = AVFMT_FLAG_KEEP_SIDE_DATA
}

-- avfmt_flag_fast_seek = AVFMT_FLAG_FAST_SEEK

-- | AVFMT_AVOID_NEG_TS_ constants
newtype AVFMTAvoidNegTS = AVFMTAvoidNegTS CInt deriving (Eq, Show, CEnum)
#{enum AVFMTAvoidNegTS, AVFMTAvoidNegTS,
	avfmt_avoid_neg_ts_auto = AVFMT_AVOID_NEG_TS_AUTO,
	avfmt_avoid_neg_ts_make_non_negative = AVFMT_AVOID_NEG_TS_MAKE_NON_NEGATIVE,
	avfmt_avoid_neg_ts_make_zero = AVFMT_AVOID_NEG_TS_MAKE_ZERO
}

-- | AVSEEK_FLAG_ flags
newtype AVSeekFlag = AVSeekFlag CInt deriving (Eq, Show, CEnum, CFlags)
#{enum AVSeekFlag, AVSeekFlag,
	avseek_flag_backward = AVSEEK_FLAG_BACKWARD,
	avseek_flag_byte = AVSEEK_FLAG_BYTE,
	avseek_flag_any = AVSEEK_FLAG_ANY,
	avseek_flag_frame = AVSEEK_FLAG_FRAME
}

