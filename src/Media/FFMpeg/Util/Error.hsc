{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
	averror_bsf_not_found,
	averror_bug,
	averror_buffer_too_small,
	averror_decoder_not_found,
	averror_demuxer_not_found,
	averror_encoder_not_found,
	averror_eof,
	averror_exit,
	averror_external,
	averror_filter_not_found,
	averror_invaliddata,
	averror_muxer_not_found,
	averror_option_not_found,
	averror_patchwelcome,
	averror_protocol_not_found,
	averror_stream_not_found,
	averror_bug2,
	averror_unknown,
	averror_experimental,
	averror_input_changed,
	averror_output_changed,
	averror_http_bad_request,
	averror_http_unauthorized,
	averror_http_forbidden,
	averror_http_not_found,
	averror_http_other_4xx,
	averror_http_server_error,

	av_error_max_string_size,
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
#{enum AVERROR, AVERROR,
	averror_bsf_not_found = AVERROR_BSF_NOT_FOUND,
	averror_bug = AVERROR_BUG,
	averror_buffer_too_small = AVERROR_BUFFER_TOO_SMALL,
	averror_decoder_not_found = AVERROR_DECODER_NOT_FOUND,
	averror_demuxer_not_found = AVERROR_DEMUXER_NOT_FOUND,
	averror_encoder_not_found = AVERROR_ENCODER_NOT_FOUND,
	averror_eof = AVERROR_EOF,
	averror_exit = AVERROR_EXIT,
	averror_external = AVERROR_EXTERNAL,
	averror_filter_not_found = AVERROR_FILTER_NOT_FOUND,
	averror_invaliddata = AVERROR_INVALIDDATA,
	averror_muxer_not_found = AVERROR_MUXER_NOT_FOUND,
	averror_option_not_found = AVERROR_OPTION_NOT_FOUND,
	averror_patchwelcome = AVERROR_PATCHWELCOME,
	averror_protocol_not_found = AVERROR_PROTOCOL_NOT_FOUND,
	averror_stream_not_found = AVERROR_STREAM_NOT_FOUND,
	averror_bug2 = AVERROR_BUG2,
	averror_unknown = AVERROR_UNKNOWN,
	averror_experimental = AVERROR_EXPERIMENTAL,
	averror_input_changed = AVERROR_INPUT_CHANGED,
	averror_output_changed = AVERROR_OUTPUT_CHANGED,
	averror_http_bad_request = AVERROR_HTTP_BAD_REQUEST,
	averror_http_unauthorized = AVERROR_HTTP_UNAUTHORIZED,
	averror_http_forbidden = AVERROR_HTTP_FORBIDDEN,
	averror_http_not_found = AVERROR_HTTP_NOT_FOUND,
	averror_http_other_4xx = AVERROR_HTTP_OTHER_4XX,
	averror_http_server_error = AVERROR_HTTP_SERVER_ERROR
}

av_error_max_string_size = #{const AV_ERROR_MAX_STRING_SIZE}

-- | Convert an error code to a string
errorToString :: AVERROR -> String
errorToString e = unsafePerformIO$
	allocaBytes (fromIntegral av_error_max_string_size)$ \pbuff -> do
		av_strerror (fromCEnum e) pbuff av_error_max_string_size
		peekCString pbuff

