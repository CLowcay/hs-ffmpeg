module Media.FFMpeg.Format.Names (
	format_avioflags,
	format_direct,
	format_probesize,
	format_formatprobesize,
	format_packetsize,
	format_fflags,
	format_flush_packets,
	format_ignidx,
	format_genpts,
	format_nofillin,
	format_noparse,
	format_igndts,
	format_discardcorrupt,
	format_sortdts,
	format_keepside,
	format_latm,
	format_nobuffer,
	format_seek2any,
	format_bitexact,
	format_analyzeduration,
	format_cryptokey,
	format_indexmem,
	format_rtbufsize,
	format_fdebug,
	format_ts,
	format_max_delay,
	format_start_time_realtime,
	format_fpsprobesize,
	format_audio_preload,
	format_chunk_duration,
	format_chunk_size,
	format_f_err_detect,
	format_err_detect,
	format_crccheck,
	format_bitstream,
	format_buffer,
	format_explode,
	format_ignore_err,
	format_careful,
	format_compliant,
	format_aggressive,
	format_use_wallclock_as_timestamps,
	format_skip_initial_bytes,
	format_correct_ts_overflow,
	format_metadata_header_padding,
	format_output_ts_offset,
	format_max_interleave_delta,
	format_f_strict,
	format_strict,
	format_normal,
	format_unofficial,
	format_experimental,
	format_max_ts_probe,
	format_avoid_negative_ts,
	format_auto,
	format_disabled,
	format_make_non_negative,
	format_make_zero,
	format_dump_separator,
	format_codec_whitelist,
	format_format_whitelist
) where

import Foreign.C.Types
import Data.Int

import Media.FFMpeg.Format.Core
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- AVFormatContext:
-- ===========================

-- | Option "avioflags" for AVFormatContext of type av_opt_type_flags-avioflags.
-- default value is AVOptionFlags 0.
format_avioflags :: OptionName AVFormatContext CInt
format_avioflags = OptionName "avioflags"

-- | Option "direct" for AVFormatContext of type av_opt_type_flags-avioflags.
-- reduce buffering
-- default value is AVOptionFlags 32768.
format_direct :: OptionName AVFormatContext CInt
format_direct = OptionName "direct"

-- | Option "probesize" for AVFormatContext of type av_opt_type_flags.
-- set probing size
-- default value is AVOptionFlags 5000000.
format_probesize :: OptionName AVFormatContext CInt
format_probesize = OptionName "probesize"

-- | Option "formatprobesize" for AVFormatContext of type av_opt_type_flags.
-- number of bytes to probe file format
-- default value is AVOptionFlags 1048576.
format_formatprobesize :: OptionName AVFormatContext CInt
format_formatprobesize = OptionName "formatprobesize"

-- | Option "packetsize" for AVFormatContext of type av_opt_type_flags.
-- set packet size
-- default value is AVOptionFlags 0.
format_packetsize :: OptionName AVFormatContext CInt
format_packetsize = OptionName "packetsize"

-- | Option "fflags" for AVFormatContext of type av_opt_type_flags-fflags.
-- default value is AVOptionFlags 512.
format_fflags :: OptionName AVFormatContext CInt
format_fflags = OptionName "fflags"

-- | Option "flush_packets" for AVFormatContext of type av_opt_type_flags-fflags.
-- reduce the latency by flushing out packets immediately
-- default value is AVOptionFlags 512.
format_flush_packets :: OptionName AVFormatContext CInt
format_flush_packets = OptionName "flush_packets"

-- | Option "ignidx" for AVFormatContext of type av_opt_type_flags-fflags.
-- ignore index
-- default value is AVOptionFlags 2.
format_ignidx :: OptionName AVFormatContext CInt
format_ignidx = OptionName "ignidx"

-- | Option "genpts" for AVFormatContext of type av_opt_type_flags-fflags.
-- generate pts
-- default value is AVOptionFlags 1.
format_genpts :: OptionName AVFormatContext CInt
format_genpts = OptionName "genpts"

-- | Option "nofillin" for AVFormatContext of type av_opt_type_flags-fflags.
-- do not fill in missing values that can be exactly calculated
-- default value is AVOptionFlags 16.
format_nofillin :: OptionName AVFormatContext CInt
format_nofillin = OptionName "nofillin"

-- | Option "noparse" for AVFormatContext of type av_opt_type_flags-fflags.
-- disable AVParsers, this needs nofillin too
-- default value is AVOptionFlags 32.
format_noparse :: OptionName AVFormatContext CInt
format_noparse = OptionName "noparse"

-- | Option "igndts" for AVFormatContext of type av_opt_type_flags-fflags.
-- ignore dts
-- default value is AVOptionFlags 8.
format_igndts :: OptionName AVFormatContext CInt
format_igndts = OptionName "igndts"

-- | Option "discardcorrupt" for AVFormatContext of type av_opt_type_flags-fflags.
-- discard corrupted frames
-- default value is AVOptionFlags 256.
format_discardcorrupt :: OptionName AVFormatContext CInt
format_discardcorrupt = OptionName "discardcorrupt"

-- | Option "sortdts" for AVFormatContext of type av_opt_type_flags-fflags.
-- try to interleave outputted packets by dts
-- default value is AVOptionFlags 65536.
format_sortdts :: OptionName AVFormatContext CInt
format_sortdts = OptionName "sortdts"

-- | Option "keepside" for AVFormatContext of type av_opt_type_flags-fflags.
-- don't merge side data
-- default value is AVOptionFlags 262144.
format_keepside :: OptionName AVFormatContext CInt
format_keepside = OptionName "keepside"

-- | Option "latm" for AVFormatContext of type av_opt_type_flags-fflags.
-- enable RTP MP4A-LATM payload
-- default value is AVOptionFlags 32768.
format_latm :: OptionName AVFormatContext CInt
format_latm = OptionName "latm"

-- | Option "nobuffer" for AVFormatContext of type av_opt_type_flags-fflags.
-- reduce the latency introduced by optional buffering
-- default value is AVOptionFlags 64.
format_nobuffer :: OptionName AVFormatContext CInt
format_nobuffer = OptionName "nobuffer"

-- | Option "seek2any" for AVFormatContext of type av_opt_type_flags.
-- allow seeking to non-keyframes on demuxer level when supported
-- default value is AVOptionFlags 0.
format_seek2any :: OptionName AVFormatContext CInt
format_seek2any = OptionName "seek2any"

-- | Option "bitexact" for AVFormatContext of type av_opt_type_flags-fflags.
-- do not write random/volatile data
-- default value is AVOptionFlags 1024.
format_bitexact :: OptionName AVFormatContext CInt
format_bitexact = OptionName "bitexact"

-- | Option "analyzeduration" for AVFormatContext of type av_opt_type_flags.
-- specify how many microseconds are analyzed to probe the input
-- default value is AVOptionFlags 0.
format_analyzeduration :: OptionName AVFormatContext CInt
format_analyzeduration = OptionName "analyzeduration"

-- | Option "cryptokey" for AVFormatContext of type av_opt_type_flags.
-- decryption key
-- default value is AVOptionFlags 0.
format_cryptokey :: OptionName AVFormatContext CInt
format_cryptokey = OptionName "cryptokey"

-- | Option "indexmem" for AVFormatContext of type av_opt_type_flags.
-- max memory used for timestamp index (per stream)
-- default value is AVOptionFlags 1048576.
format_indexmem :: OptionName AVFormatContext CInt
format_indexmem = OptionName "indexmem"

-- | Option "rtbufsize" for AVFormatContext of type av_opt_type_flags.
-- max memory used for buffering real-time frames
-- default value is AVOptionFlags 3041280.
format_rtbufsize :: OptionName AVFormatContext CInt
format_rtbufsize = OptionName "rtbufsize"

-- | Option "fdebug" for AVFormatContext of type av_opt_type_flags-fdebug.
-- print specific debug info
-- default value is AVOptionFlags 0.
format_fdebug :: OptionName AVFormatContext CInt
format_fdebug = OptionName "fdebug"

-- | Option "ts" for AVFormatContext of type av_opt_type_flags-fdebug.
-- default value is AVOptionFlags 1.
format_ts :: OptionName AVFormatContext CInt
format_ts = OptionName "ts"

-- | Option "max_delay" for AVFormatContext of type av_opt_type_flags.
-- maximum muxing or demuxing delay in microseconds
-- default value is AVOptionFlags (-1).
format_max_delay :: OptionName AVFormatContext CInt
format_max_delay = OptionName "max_delay"

-- | Option "start_time_realtime" for AVFormatContext of type av_opt_type_flags.
-- wall-clock time when stream begins (PTS==0)
-- default value is AVOptionFlags 0.
format_start_time_realtime :: OptionName AVFormatContext CInt
format_start_time_realtime = OptionName "start_time_realtime"

-- | Option "fpsprobesize" for AVFormatContext of type av_opt_type_flags.
-- number of frames used to probe fps
-- default value is AVOptionFlags (-1).
format_fpsprobesize :: OptionName AVFormatContext CInt
format_fpsprobesize = OptionName "fpsprobesize"

-- | Option "audio_preload" for AVFormatContext of type av_opt_type_flags.
-- microseconds by which audio packets should be interleaved earlier
-- default value is AVOptionFlags 0.
format_audio_preload :: OptionName AVFormatContext CInt
format_audio_preload = OptionName "audio_preload"

-- | Option "chunk_duration" for AVFormatContext of type av_opt_type_flags.
-- microseconds for each chunk
-- default value is AVOptionFlags 0.
format_chunk_duration :: OptionName AVFormatContext CInt
format_chunk_duration = OptionName "chunk_duration"

-- | Option "chunk_size" for AVFormatContext of type av_opt_type_flags.
-- size in bytes for each chunk
-- default value is AVOptionFlags 0.
format_chunk_size :: OptionName AVFormatContext CInt
format_chunk_size = OptionName "chunk_size"

-- | Option "f_err_detect" for AVFormatContext of type av_opt_type_flags-err_detect.
-- set error detection flags (deprecated; use err_detect, save via avconv)
-- default value is AVOptionFlags 1.
format_f_err_detect :: OptionName AVFormatContext CInt
format_f_err_detect = OptionName "f_err_detect"

-- | Option "err_detect" for AVFormatContext of type av_opt_type_flags-err_detect.
-- set error detection flags
-- default value is AVOptionFlags 1.
format_err_detect :: OptionName AVFormatContext CInt
format_err_detect = OptionName "err_detect"

-- | Option "crccheck" for AVFormatContext of type av_opt_type_flags-err_detect.
-- verify embedded CRCs
-- default value is AVOptionFlags 1.
format_crccheck :: OptionName AVFormatContext CInt
format_crccheck = OptionName "crccheck"

-- | Option "bitstream" for AVFormatContext of type av_opt_type_flags-err_detect.
-- detect bitstream specification deviations
-- default value is AVOptionFlags 2.
format_bitstream :: OptionName AVFormatContext CInt
format_bitstream = OptionName "bitstream"

-- | Option "buffer" for AVFormatContext of type av_opt_type_flags-err_detect.
-- detect improper bitstream length
-- default value is AVOptionFlags 4.
format_buffer :: OptionName AVFormatContext CInt
format_buffer = OptionName "buffer"

-- | Option "explode" for AVFormatContext of type av_opt_type_flags-err_detect.
-- abort decoding on minor error detection
-- default value is AVOptionFlags 8.
format_explode :: OptionName AVFormatContext CInt
format_explode = OptionName "explode"

-- | Option "ignore_err" for AVFormatContext of type av_opt_type_flags-err_detect.
-- ignore errors
-- default value is AVOptionFlags 32768.
format_ignore_err :: OptionName AVFormatContext CInt
format_ignore_err = OptionName "ignore_err"

-- | Option "careful" for AVFormatContext of type av_opt_type_flags-err_detect.
-- consider things that violate the spec, are fast to check and have not been
-- seen in the wild as errors
-- default value is AVOptionFlags 65536.
format_careful :: OptionName AVFormatContext CInt
format_careful = OptionName "careful"

-- | Option "compliant" for AVFormatContext of type av_opt_type_flags-err_detect.
-- consider all spec non compliancies as errors
-- default value is AVOptionFlags 131072.
format_compliant :: OptionName AVFormatContext CInt
format_compliant = OptionName "compliant"

-- | Option "aggressive" for AVFormatContext of type av_opt_type_flags-err_detect.
-- consider things that a sane encoder shouldn't do as an error
-- default value is AVOptionFlags 262144.
format_aggressive :: OptionName AVFormatContext CInt
format_aggressive = OptionName "aggressive"

-- | Option "use_wallclock_as_timestamps" for AVFormatContext of type av_opt_type_flags.
-- use wallclock as timestamps
-- default value is AVOptionFlags 0.
format_use_wallclock_as_timestamps :: OptionName AVFormatContext CInt
format_use_wallclock_as_timestamps = OptionName "use_wallclock_as_timestamps"

-- | Option "skip_initial_bytes" for AVFormatContext of type av_opt_type_flags.
-- set number of bytes to skip before reading header and frames
-- default value is AVOptionFlags 0.
format_skip_initial_bytes :: OptionName AVFormatContext CInt
format_skip_initial_bytes = OptionName "skip_initial_bytes"

-- | Option "correct_ts_overflow" for AVFormatContext of type av_opt_type_flags.
-- correct single timestamp overflows
-- default value is AVOptionFlags 1.
format_correct_ts_overflow :: OptionName AVFormatContext CInt
format_correct_ts_overflow = OptionName "correct_ts_overflow"

-- | Option "metadata_header_padding" for AVFormatContext of type av_opt_type_flags.
-- set number of bytes to be written as padding in a metadata header
-- default value is AVOptionFlags (-1).
format_metadata_header_padding :: OptionName AVFormatContext CInt
format_metadata_header_padding = OptionName "metadata_header_padding"

-- | Option "output_ts_offset" for AVFormatContext of type av_opt_type_flags.
-- set output timestamp offset
-- default value is AVOptionFlags 0.
format_output_ts_offset :: OptionName AVFormatContext CInt
format_output_ts_offset = OptionName "output_ts_offset"

-- | Option "max_interleave_delta" for AVFormatContext of type av_opt_type_flags.
-- maximum buffering duration for interleaving
-- default value is AVOptionFlags 10000000.
format_max_interleave_delta :: OptionName AVFormatContext CInt
format_max_interleave_delta = OptionName "max_interleave_delta"

-- | Option "f_strict" for AVFormatContext of type av_opt_type_flags-strict.
-- how strictly to follow the standards (deprecated; use strict, save via
-- avconv)
-- default value is AVOptionFlags 0.
format_f_strict :: OptionName AVFormatContext CInt
format_f_strict = OptionName "f_strict"

-- | Option "strict" for AVFormatContext of type av_opt_type_flags-strict.
-- how strictly to follow the standards
-- default value is AVOptionFlags 0.
format_strict :: OptionName AVFormatContext CInt
format_strict = OptionName "strict"

-- | Option "normal" for AVFormatContext of type av_opt_type_flags-strict.
-- default value is AVOptionFlags 0.
format_normal :: OptionName AVFormatContext CInt
format_normal = OptionName "normal"

-- | Option "unofficial" for AVFormatContext of type av_opt_type_flags-strict.
-- allow unofficial extensions
-- default value is AVOptionFlags (-1).
format_unofficial :: OptionName AVFormatContext CInt
format_unofficial = OptionName "unofficial"

-- | Option "experimental" for AVFormatContext of type av_opt_type_flags-strict.
-- allow non-standardized experimental variants
-- default value is AVOptionFlags (-2).
format_experimental :: OptionName AVFormatContext CInt
format_experimental = OptionName "experimental"

-- | Option "max_ts_probe" for AVFormatContext of type av_opt_type_flags.
-- maximum number of packets to read while waiting for the first timestamp
-- default value is AVOptionFlags 50.
format_max_ts_probe :: OptionName AVFormatContext CInt
format_max_ts_probe = OptionName "max_ts_probe"

-- | Option "avoid_negative_ts" for AVFormatContext of type av_opt_type_flags-avoid_negative_ts.
-- shift timestamps so they start at 0
-- default value is AVOptionFlags (-1).
format_avoid_negative_ts :: OptionName AVFormatContext CInt
format_avoid_negative_ts = OptionName "avoid_negative_ts"

-- | Option "auto" for AVFormatContext of type av_opt_type_flags-avoid_negative_ts.
-- enabled when required by target format
-- default value is AVOptionFlags (-1).
format_auto :: OptionName AVFormatContext CInt
format_auto = OptionName "auto"

-- | Option "disabled" for AVFormatContext of type av_opt_type_flags-avoid_negative_ts.
-- do not change timestamps
-- default value is AVOptionFlags 0.
format_disabled :: OptionName AVFormatContext CInt
format_disabled = OptionName "disabled"

-- | Option "make_non_negative" for AVFormatContext of type av_opt_type_flags-avoid_negative_ts.
-- shift timestamps so they are non negative
-- default value is AVOptionFlags 1.
format_make_non_negative :: OptionName AVFormatContext CInt
format_make_non_negative = OptionName "make_non_negative"

-- | Option "make_zero" for AVFormatContext of type av_opt_type_flags-avoid_negative_ts.
-- shift timestamps so they start at 0
-- default value is AVOptionFlags 2.
format_make_zero :: OptionName AVFormatContext CInt
format_make_zero = OptionName "make_zero"

-- | Option "dump_separator" for AVFormatContext of type av_opt_type_flags.
-- set information dump field separator
-- default value is AVOptionFlags 2084899653.
format_dump_separator :: OptionName AVFormatContext CInt
format_dump_separator = OptionName "dump_separator"

-- | Option "codec_whitelist" for AVFormatContext of type av_opt_type_flags.
-- List of decoders that are allowed to be used
-- default value is AVOptionFlags 0.
format_codec_whitelist :: OptionName AVFormatContext CInt
format_codec_whitelist = OptionName "codec_whitelist"

-- | Option "format_whitelist" for AVFormatContext of type av_opt_type_flags.
-- List of demuxers that are allowed to be used
-- default value is AVOptionFlags 0.
format_format_whitelist :: OptionName AVFormatContext CInt
format_format_whitelist = OptionName "format_whitelist"

