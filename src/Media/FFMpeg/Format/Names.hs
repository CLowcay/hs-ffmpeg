{- |
 
Description : Named options for livavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Named options for livavformat

-}
module Media.FFMpeg.Format.Names (
	format_avioflags,
	format_probesize,
	format_formatprobesize,
	format_packetsize,
	format_fflags,
	format_seek2any,
	format_analyzeduration,
	format_cryptokey,
	format_indexmem,
	format_rtbufsize,
	format_fdebug,
	format_max_delay,
	format_start_time_realtime,
	format_fpsprobesize,
	format_audio_preload,
	format_chunk_duration,
	format_chunk_size,
	format_f_err_detect,
	format_err_detect,
	format_use_wallclock_as_timestamps,
	format_skip_initial_bytes,
	format_correct_ts_overflow,
	format_flush_packets,
	format_metadata_header_padding,
	format_output_ts_offset,
	format_max_interleave_delta,
	format_f_strict,
	format_strict,
	format_max_ts_probe,
	format_avoid_negative_ts,
	format_dump_separator,
	format_codec_whitelist,
	format_format_whitelist
) where

import Data.Int
import Foreign.C.Types
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util
import qualified Data.ByteString as B

-- AVFormatContext:
-- ===========================

-- | Option "avioflags" for AVFormatContext of type AVOptTypeFlags-avioflags.
-- default value is AVOptionFlags 0.
format_avioflags :: OptionName AVFormatContext CInt
format_avioflags = OptionName "avioflags"

-- | Option "probesize" for AVFormatContext of type AVOptTypeInt64.
-- set probing size
-- default value is AVOptionInt64 5000000.
format_probesize :: OptionName AVFormatContext Int64
format_probesize = OptionName "probesize"

-- | Option "formatprobesize" for AVFormatContext of type AVOptTypeInt.
-- number of bytes to probe file format
-- default value is AVOptionInt 1048576.
format_formatprobesize :: OptionName AVFormatContext CInt
format_formatprobesize = OptionName "formatprobesize"

-- | Option "packetsize" for AVFormatContext of type AVOptTypeInt.
-- set packet size
-- default value is AVOptionInt 0.
format_packetsize :: OptionName AVFormatContext CInt
format_packetsize = OptionName "packetsize"

-- | Option "fflags" for AVFormatContext of type AVOptTypeFlags-fflags.
-- default value is AVOptionFlags 512.
format_fflags :: OptionName AVFormatContext CInt
format_fflags = OptionName "fflags"

-- | Option "seek2any" for AVFormatContext of type AVOptTypeInt.
-- allow seeking to non-keyframes on demuxer level when supported
-- default value is AVOptionInt 0.
format_seek2any :: OptionName AVFormatContext CInt
format_seek2any = OptionName "seek2any"

-- | Option "analyzeduration" for AVFormatContext of type AVOptTypeInt64.
-- specify how many microseconds are analyzed to probe the input
-- default value is AVOptionInt64 0.
format_analyzeduration :: OptionName AVFormatContext Int64
format_analyzeduration = OptionName "analyzeduration"

-- | Option "cryptokey" for AVFormatContext of type AVOptTypeBinary.
-- decryption key
format_cryptokey :: OptionName AVFormatContext B.ByteString
format_cryptokey = OptionName "cryptokey"

-- | Option "indexmem" for AVFormatContext of type AVOptTypeInt.
-- max memory used for timestamp index (per stream)
-- default value is AVOptionInt 1048576.
format_indexmem :: OptionName AVFormatContext CInt
format_indexmem = OptionName "indexmem"

-- | Option "rtbufsize" for AVFormatContext of type AVOptTypeInt.
-- max memory used for buffering real-time frames
-- default value is AVOptionInt 3041280.
format_rtbufsize :: OptionName AVFormatContext CInt
format_rtbufsize = OptionName "rtbufsize"

-- | Option "fdebug" for AVFormatContext of type AVOptTypeFlags-fdebug.
-- print specific debug info
-- default value is AVOptionFlags 0.
format_fdebug :: OptionName AVFormatContext CInt
format_fdebug = OptionName "fdebug"

-- | Option "max_delay" for AVFormatContext of type AVOptTypeInt.
-- maximum muxing or demuxing delay in microseconds
-- default value is AVOptionInt (-1).
format_max_delay :: OptionName AVFormatContext CInt
format_max_delay = OptionName "max_delay"

-- | Option "start_time_realtime" for AVFormatContext of type AVOptTypeInt64.
-- wall-clock time when stream begins (PTS==0)
-- default value is AVOptionInt64 (-9223372036854775808).
format_start_time_realtime :: OptionName AVFormatContext Int64
format_start_time_realtime = OptionName "start_time_realtime"

-- | Option "fpsprobesize" for AVFormatContext of type AVOptTypeInt.
-- number of frames used to probe fps
-- default value is AVOptionInt (-1).
format_fpsprobesize :: OptionName AVFormatContext CInt
format_fpsprobesize = OptionName "fpsprobesize"

-- | Option "audio_preload" for AVFormatContext of type AVOptTypeInt.
-- microseconds by which audio packets should be interleaved earlier
-- default value is AVOptionInt 0.
format_audio_preload :: OptionName AVFormatContext CInt
format_audio_preload = OptionName "audio_preload"

-- | Option "chunk_duration" for AVFormatContext of type AVOptTypeInt.
-- microseconds for each chunk
-- default value is AVOptionInt 0.
format_chunk_duration :: OptionName AVFormatContext CInt
format_chunk_duration = OptionName "chunk_duration"

-- | Option "chunk_size" for AVFormatContext of type AVOptTypeInt.
-- size in bytes for each chunk
-- default value is AVOptionInt 0.
format_chunk_size :: OptionName AVFormatContext CInt
format_chunk_size = OptionName "chunk_size"

-- | Option "f_err_detect" for AVFormatContext of type AVOptTypeFlags-err_detect.
-- set error detection flags (deprecated; use err_detect, save via avconv)
-- default value is AVOptionFlags 1.
format_f_err_detect :: OptionName AVFormatContext CInt
format_f_err_detect = OptionName "f_err_detect"

-- | Option "err_detect" for AVFormatContext of type AVOptTypeFlags-err_detect.
-- set error detection flags
-- default value is AVOptionFlags 1.
format_err_detect :: OptionName AVFormatContext CInt
format_err_detect = OptionName "err_detect"

-- | Option "use_wallclock_as_timestamps" for AVFormatContext of type AVOptTypeInt.
-- use wallclock as timestamps
-- default value is AVOptionInt 0.
format_use_wallclock_as_timestamps :: OptionName AVFormatContext CInt
format_use_wallclock_as_timestamps = OptionName "use_wallclock_as_timestamps"

-- | Option "skip_initial_bytes" for AVFormatContext of type AVOptTypeInt64.
-- set number of bytes to skip before reading header and frames
-- default value is AVOptionInt64 0.
format_skip_initial_bytes :: OptionName AVFormatContext Int64
format_skip_initial_bytes = OptionName "skip_initial_bytes"

-- | Option "correct_ts_overflow" for AVFormatContext of type AVOptTypeInt.
-- correct single timestamp overflows
-- default value is AVOptionInt 1.
format_correct_ts_overflow :: OptionName AVFormatContext CInt
format_correct_ts_overflow = OptionName "correct_ts_overflow"

-- | Option "flush_packets" for AVFormatContext of type AVOptTypeInt.
-- enable flushing of the I/O context after each packet
-- default value is AVOptionInt 1.
format_flush_packets :: OptionName AVFormatContext CInt
format_flush_packets = OptionName "flush_packets"

-- | Option "metadata_header_padding" for AVFormatContext of type AVOptTypeInt.
-- set number of bytes to be written as padding in a metadata header
-- default value is AVOptionInt (-1).
format_metadata_header_padding :: OptionName AVFormatContext CInt
format_metadata_header_padding = OptionName "metadata_header_padding"

-- | Option "output_ts_offset" for AVFormatContext of type AVOptTypeDuration.
-- set output timestamp offset
format_output_ts_offset :: OptionName AVFormatContext Int64
format_output_ts_offset = OptionName "output_ts_offset"

-- | Option "max_interleave_delta" for AVFormatContext of type AVOptTypeInt64.
-- maximum buffering duration for interleaving
-- default value is AVOptionInt64 10000000.
format_max_interleave_delta :: OptionName AVFormatContext Int64
format_max_interleave_delta = OptionName "max_interleave_delta"

-- | Option "f_strict" for AVFormatContext of type AVOptTypeInt-strict.
-- how strictly to follow the standards (deprecated; use strict, save via
-- avconv)
-- default value is AVOptionInt 0.
format_f_strict :: OptionName AVFormatContext CInt
format_f_strict = OptionName "f_strict"

-- | Option "strict" for AVFormatContext of type AVOptTypeInt-strict.
-- how strictly to follow the standards
-- default value is AVOptionInt 0.
format_strict :: OptionName AVFormatContext CInt
format_strict = OptionName "strict"

-- | Option "max_ts_probe" for AVFormatContext of type AVOptTypeInt.
-- maximum number of packets to read while waiting for the first timestamp
-- default value is AVOptionInt 50.
format_max_ts_probe :: OptionName AVFormatContext CInt
format_max_ts_probe = OptionName "max_ts_probe"

-- | Option "avoid_negative_ts" for AVFormatContext of type AVOptTypeInt-avoid_negative_ts.
-- shift timestamps so they start at 0
-- default value is AVOptionInt (-1).
format_avoid_negative_ts :: OptionName AVFormatContext CInt
format_avoid_negative_ts = OptionName "avoid_negative_ts"

-- | Option "dump_separator" for AVFormatContext of type AVOptTypeString.
-- set information dump field separator
-- default value is AVOptionString ", ".
format_dump_separator :: OptionName AVFormatContext String
format_dump_separator = OptionName "dump_separator"

-- | Option "codec_whitelist" for AVFormatContext of type AVOptTypeString.
-- List of decoders that are allowed to be used
format_codec_whitelist :: OptionName AVFormatContext String
format_codec_whitelist = OptionName "codec_whitelist"

-- | Option "format_whitelist" for AVFormatContext of type AVOptTypeString.
-- List of demuxers that are allowed to be used
format_format_whitelist :: OptionName AVFormatContext String
format_format_whitelist = OptionName "format_whitelist"

