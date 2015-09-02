{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |

Module      : 
Description : ffmpeg error codes
Copyright   : Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

ffmpeg error codes

-}

module Media.FFMpeg.Util.Error (
	AVERROR,
	pattern AVERRORBsfNotFound,
	pattern AVERRORBug,
	pattern AVERRORBufferTooSmall,
	pattern AVERRORDecoderNotFound,
	pattern AVERRORDemuxerNotFound,
	pattern AVERROREncoderNotFound,
	pattern AVERROREof,
	pattern AVERRORExit,
	pattern AVERRORExternal,
	pattern AVERRORFilterNot_found,
	pattern AVERRORInvaliddata,
	pattern AVERRORMuxerNotFound,
	pattern AVERROROptionNotFound,
	pattern AVERRORPatchwelcome,
	pattern AVERRORProtocolNotFound,
	pattern AVERRORStreamNotFound,
	pattern AVERRORBug2,
	pattern AVERRORUnknown,
	pattern AVERRORExperimental,
	pattern AVERRORInputChanged,
	pattern AVERROROutputChanged,
	pattern AVERRORHttpBadRequest,
	pattern AVERRORHttpUnauthorized,
	pattern AVERRORHttpForbidden,
	pattern AVERRORHttpNotFound,
	pattern AVERRORHttpOther4xx,
	pattern AVERRORHttpServerError,

	errorToString
) where

#include "ffmpeg.h"

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import System.IO.Unsafe

import Media.FFMpeg.Internal.Common

foreign import ccall "av_strerror" av_strerror :: CInt -> CString -> CSize -> IO CInt

newtype AVERROR = AVERROR CInt deriving (Eq, CEnum)
pattern AVERRORBsfNotFound = AVERROR (#{const AVERROR_BSF_NOT_FOUND})
pattern AVERRORBug = AVERROR (#{const AVERROR_BUG})
pattern AVERRORBufferTooSmall = AVERROR (#{const AVERROR_BUFFER_TOO_SMALL})
pattern AVERRORDecoderNotFound = AVERROR (#{const AVERROR_DECODER_NOT_FOUND})
pattern AVERRORDemuxerNotFound = AVERROR (#{const AVERROR_DEMUXER_NOT_FOUND})
pattern AVERROREncoderNotFound = AVERROR (#{const AVERROR_ENCODER_NOT_FOUND})
pattern AVERROREof = AVERROR (#{const AVERROR_EOF})
pattern AVERRORExit = AVERROR (#{const AVERROR_EXIT})
pattern AVERRORExternal = AVERROR (#{const AVERROR_EXTERNAL})
pattern AVERRORFilterNot_found = AVERROR (#{const AVERROR_FILTER_NOT_FOUND})
pattern AVERRORInvaliddata = AVERROR (#{const AVERROR_INVALIDDATA})
pattern AVERRORMuxerNotFound = AVERROR (#{const AVERROR_MUXER_NOT_FOUND})
pattern AVERROROptionNotFound = AVERROR (#{const AVERROR_OPTION_NOT_FOUND})
pattern AVERRORPatchwelcome = AVERROR (#{const AVERROR_PATCHWELCOME})
pattern AVERRORProtocolNotFound = AVERROR (#{const AVERROR_PROTOCOL_NOT_FOUND})
pattern AVERRORStreamNotFound = AVERROR (#{const AVERROR_STREAM_NOT_FOUND})
pattern AVERRORBug2 = AVERROR (#{const AVERROR_BUG2})
pattern AVERRORUnknown = AVERROR (#{const AVERROR_UNKNOWN})
pattern AVERRORExperimental = AVERROR (#{const AVERROR_EXPERIMENTAL})
pattern AVERRORInputChanged = AVERROR (#{const AVERROR_INPUT_CHANGED})
pattern AVERROROutputChanged = AVERROR (#{const AVERROR_OUTPUT_CHANGED})
pattern AVERRORHttpBadRequest = AVERROR (#{const AVERROR_HTTP_BAD_REQUEST})
pattern AVERRORHttpUnauthorized = AVERROR (#{const AVERROR_HTTP_UNAUTHORIZED})
pattern AVERRORHttpForbidden = AVERROR (#{const AVERROR_HTTP_FORBIDDEN})
pattern AVERRORHttpNotFound = AVERROR (#{const AVERROR_HTTP_NOT_FOUND})
pattern AVERRORHttpOther4xx = AVERROR (#{const AVERROR_HTTP_OTHER_4XX})
pattern AVERRORHttpServerError = AVERROR (#{const AVERROR_HTTP_SERVER_ERROR})

-- | Convert an error code to a string
errorToString :: AVERROR -> String
errorToString e = unsafePerformIO$
	allocaBytes (fromIntegral av_error_max_string_size)$ \pbuff -> do
		av_strerror (fromCEnum e) pbuff av_error_max_string_size
		peekCString pbuff
	where av_error_max_string_size = #{const AV_ERROR_MAX_STRING_SIZE}

