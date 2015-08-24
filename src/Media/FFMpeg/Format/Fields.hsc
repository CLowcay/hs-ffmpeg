{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Fields (
	avstream_index,
	avstream_id,
	avstream_codec,
	avstream_time_base,
	avstream_start_time,
	avstream_duration,
	avstream_nb_frames,
	avstream_disposition,
	avstream_discard,
	avstream_sample_aspect_ratio,
	avstream_metadata,
	avstream_avg_frame_rate,
	avstream_attached_pic,
	avstream_event_flags,

	formatGetInputFormat,
	formatSetOutputFormat,
	format_ctx_flags,
	formatGetFilename,
	formatSetFilename,
	format_start_time,
	format_duration,
	format_bit_rate,
	format_packet_size,
	format_flags,
	format_video_codec_id,
	format_audio_codec_id,
	format_subtitle_codec_id,
	format_max_index_size,
	format_max_picture_buffer,
	format_metadata,
	format_fps_probe_size,
	format_error_recognition,
	format_debug,
	format_strict_std_compliance,
	format_event_flags
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Util.Dict

avstream_index :: Field AVStream CInt ReadOnly
avstream_index = Field #{offset AVStream, index}

avstream_id :: Field AVStream CInt ReadOnly
avstream_id = Field #{offset AVStream, id}

avstream_codec :: Field AVStream AVCodecContext ReadOnly
avstream_codec = Field #{offset AVStream, codec}

avstream_time_base :: Field AVStream AVRational ReadWrite
avstream_time_base = Field #{offset AVStream, time_base}

avstream_start_time :: Field AVStream AVTimestamp ReadWrite
avstream_start_time = Field #{offset AVStream, start_time}

avstream_duration :: Field AVStream AVTimestamp ReadWrite
avstream_duration = Field #{offset AVStream, duration}

avstream_nb_frames :: Field AVStream Int64 ReadWrite
avstream_nb_frames = Field #{offset AVStream, nb_frames}

avstream_disposition :: Field AVStream AVDispositionFlag ReadWrite
avstream_disposition = Field #{offset AVStream, disposition}

avstream_discard :: Field AVStream AVDiscard ReadWrite
avstream_discard = Field #{offset AVStream, discard}

avstream_sample_aspect_ratio :: Field AVStream AVRational ReadWrite
avstream_sample_aspect_ratio = Field #{offset AVStream, sample_aspect_ratio}

avstream_metadata :: Field AVStream AVDictionary ReadWrite
avstream_metadata = Field #{offset AVStream, metadata}

avstream_avg_frame_rate :: Field AVStream AVRational ReadWrite
avstream_avg_frame_rate = Field #{offset AVStream, avg_frame_rate}

avstream_attached_pic :: Field AVStream AVPacket ReadOnly
avstream_attached_pic = Field #{offset AVStream, attached_pic}

avstream_event_flags :: Field AVStream AVStreamEventFlag ReadWrite
avstream_event_flags = Field #{offset AVStream, event_flags}

-- | Get the AVInputFormat (which was set by openInput)
formatGetInputFormat :: MonadIO m => AVFormatContext -> m AVInputFormat
formatGetInputFormat ctx = liftIO.withThis ctx$ \pctx ->
	AVInputFormat <$> #{peek AVFormatContext, iformat} pctx

-- | Set the AVOutputFormat (must do this before calling writeHeader)
formatSetOutputFormat :: MonadIO m => AVFormatContext -> AVOutputFormat -> m ()
formatSetOutputFormat ctx off =
	withThis ctx$ \pctx ->
	withThis off$ \pof -> liftIO$ #{poke AVFormatContext, oformat} pctx pof

format_ctx_flags :: Field AVFormatContext AVFmtCtx ReadOnly
format_ctx_flags = Field #{offset AVFormatContext, ctx_flags}

-- | Get the filename associated with the AVFormatContext
formatGetFilename :: MonadIO m => AVFormatContext -> m String
formatGetFilename ctx = withThis ctx$ \pctx ->
	liftIO.peekCString$ pctx `plusPtr` #{offset AVFormatContext, filename}

-- | Set the filename to use for this AVFormatContext (call before writeHeader)
formatSetFilename :: (MonadIO m, MonadError String m) => AVFormatContext -> String -> m ()
formatSetFilename ctx s =
	withThis ctx$ \pctx -> do
	len <- liftIO.withCStringLen s$ \(ps, len) -> do
		when (len < 1024)$ do
			moveBytes (pctx `plusPtr` #{offset AVFormatContext, filename}) ps len
			poke (pctx `plusPtr` (#{offset AVFormatContext, filename} + len)) (castCharToCChar '\0')
		return len

	when (len >= 1024)$ throwError$
		"formatSetFilename: String is too long, max length is 1023, actual length was " ++ (show len)

format_start_time :: Field AVFormatContext AVTimestamp ReadOnly
format_start_time = Field #{offset AVFormatContext, start_time}

format_duration :: Field AVFormatContext AVTimestamp ReadWrite
format_duration = Field #{offset AVFormatContext, duration}

format_bit_rate :: Field AVFormatContext CInt ReadWrite
format_bit_rate = Field #{offset AVFormatContext, bit_rate}

format_packet_size :: Field AVFormatContext CUInt ReadWrite
format_packet_size = Field #{offset AVFormatContext, packet_size}

format_max_delay :: Field AVFormatContext CInt ReadWrite
format_max_delay = Field #{offset AVFormatContext, max_delay}

format_flags :: Field AVFormatContext AVFmtFlag ReadWrite
format_flags = Field #{offset AVFormatContext, flags}

format_video_codec_id :: Field AVFormatContext AVCodecID ReadWrite
format_video_codec_id = Field #{offset AVFormatContext, video_codec_id}

format_audio_codec_id :: Field AVFormatContext AVCodecID ReadWrite
format_audio_codec_id = Field #{offset AVFormatContext, audio_codec_id}

format_subtitle_codec_id :: Field AVFormatContext AVCodecID ReadWrite
format_subtitle_codec_id = Field #{offset AVFormatContext, subtitle_codec_id}

format_max_index_size :: Field AVFormatContext CUInt ReadWrite
format_max_index_size = Field #{offset AVFormatContext, max_index_size}

format_max_picture_buffer :: Field AVFormatContext CUInt ReadWrite
format_max_picture_buffer = Field #{offset AVFormatContext, max_picture_buffer}

format_metadata :: Field AVFormatContext AVDictionary ReadWrite
format_metadata = Field #{offset AVFormatContext, metadata}

format_start_time_realtime :: Field AVFormatContext Int64 ReadWrite
format_start_time_realtime = Field #{offset AVFormatContext, start_time_realtime}

format_fps_probe_size :: Field AVFormatContext CInt ReadWrite
format_fps_probe_size = Field #{offset AVFormatContext, fps_probe_size}

format_error_recognition :: Field AVFormatContext AVEF ReadWrite
format_error_recognition = Field #{offset AVFormatContext, error_recognition}

format_debug :: Field AVFormatContext FFFDebug ReadWrite
format_debug = Field #{offset AVFormatContext, debug}

format_max_interleave_delta :: Field AVFormatContext AVTimestamp ReadWrite
format_max_interleave_delta = Field #{offset AVFormatContext, max_interleave_delta}

format_strict_std_compliance :: Field AVFormatContext FFCompliance ReadWrite
format_strict_std_compliance = Field #{offset AVFormatContext, strict_std_compliance}

format_event_flags :: Field AVFormatContext AVFmtEventFlag ReadWrite
format_event_flags = Field #{offset AVFormatContext, event_flags}

format_max_ts_probe :: Field AVFormatContext CInt ReadWrite
format_max_ts_probe = Field #{offset AVFormatContext, max_ts_probe}

format_avoid_negative_ts :: Field AVFormatContext AVFmtAvoidNegTS ReadWrite
format_avoid_negative_ts = Field #{offset AVFormatContext, avoid_negative_ts}

