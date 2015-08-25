{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to livavcodec.

-}

module Media.FFMpeg.Codec.Fields (
	codecctx_codec_type,
	codecctx_codec,
	codecctx_codec_id,
	codecctx_width,
	codecctx_height,
	codecctx_coded_width,
	codecctx_coded_height,
	codecctx_sample_rate,
	codecctx_rc_initial_buffer_occupancy,
	codecctx_stats_out,
	codecctx_reordered_opaque,
	codecctx_vbv_delay,
	codecctx_initial_padding,
	codecctx_framerate,
	--codecctx_sw_pix_fmt,
	codecctx_seek_preroll,
	codecGetSubtitleHeader,

	codec_name,
	codec_long_name,
	codec_type,
	codec_id,
	codec_capabilities,
	codec_max_lowres,
	getSupportedFramerates,
	getSupportedPixelFormats,
	getSupportedSamplerates,
	getSupportedSampleFormats,
	getSupportedChannelLayouts,
	getSupportedProfiles,
	getCodecClass
) where

#include "ffmpeg.h"

import Control.Applicative
import Data.Maybe
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B

import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Util.Options

codecctx_codec_type :: Field AVCodecContext AVMediaType ReadWrite
codecctx_codec_type = Field #{offset AVCodecContext, codec_type} []

codecctx_codec :: Field AVCodecContext AVCodec ReadWrite
codecctx_codec = Field #{offset AVCodecContext, codec} []

codecctx_codec_id :: Field AVCodecContext AVCodecID ReadWrite
codecctx_codec_id = Field #{offset AVCodecContext, codec_id} []

codecctx_width :: Field AVCodecContext CInt ReadWrite
codecctx_width = Field #{offset AVCodecContext, width} []

codecctx_height :: Field AVCodecContext CInt ReadWrite
codecctx_height = Field #{offset AVCodecContext, height} []

codecctx_coded_width :: Field AVCodecContext CInt ReadWrite
codecctx_coded_width = Field #{offset AVCodecContext, coded_width} []

codecctx_coded_height :: Field AVCodecContext CInt ReadWrite
codecctx_coded_height = Field #{offset AVCodecContext, coded_height} []

codecctx_sample_rate :: Field AVCodecContext CInt ReadWrite
codecctx_sample_rate = Field #{offset AVCodecContext, sample_rate} []

codecctx_rc_initial_buffer_occupancy :: Field AVCodecContext CInt ReadWrite
codecctx_rc_initial_buffer_occupancy = Field #{offset AVCodecContext, rc_initial_buffer_occupancy} []

codecctx_stats_out :: Field AVCodecContext String ReadOnly
codecctx_stats_out = Field #{offset AVCodecContext, stats_out} []

codecctx_reordered_opaque :: Field AVCodecContext Int64 ReadWrite
codecctx_reordered_opaque = Field #{offset AVCodecContext, reordered_opaque} []

codecctx_vbv_delay :: Field AVCodecContext Word64 ReadWrite
codecctx_vbv_delay = Field #{offset AVCodecContext, vbv_delay} []

codecctx_initial_padding :: Field AVCodecContext CInt ReadOnly
codecctx_initial_padding = Field #{offset AVCodecContext, initial_padding} []

codecctx_framerate :: Field AVCodecContext (Maybe AVRational) ReadOnly
codecctx_framerate = Field #{offset AVCodecContext, framerate} []

-- codecctx_sw_pix_fmt :: Field AVCodecContext AVPixelFormat ReadOnly
-- codecctx_sw_pix_fmt = Field #{offset AVCodecContext, sw_pix_fmt} []

codecctx_seek_preroll :: Field AVCodecContext CInt ReadOnly
codecctx_seek_preroll = Field #{offset AVCodecContext, seek_preroll} []

-- | Get the subtitle_header field from an AVCodecContext
codecGetSubtitleHeader :: MonadIO m => AVCodecContext -> m (Maybe B.ByteString)
codecGetSubtitleHeader ctx = liftIO.withThis ctx$ \pctx -> do
	pheader <- #{peek AVCodecContext, subtitle_header} pctx
	size <- #{peek AVCodecContext, subtitle_header_size} pctx
	if pheader == nullPtr then return Nothing else
		Just <$> B.packCStringLen (pheader, size)

codec_name :: Field AVCodec String ReadOnly
codec_name = Field #{offset AVCodec, name} []

codec_long_name :: Field AVCodec String ReadOnly
codec_long_name = Field #{offset AVCodec, long_name} []

codec_type :: Field AVCodec AVMediaType ReadOnly
codec_type = Field #{offset AVCodec, type} []

codec_id :: Field AVCodec AVCodecID ReadOnly
codec_id = Field #{offset AVCodec, id} []

codec_capabilities :: Field AVCodec AVCodecCap ReadOnly
codec_capabilities = Field #{offset AVCodec, capabilities} []

codec_max_lowres :: Field AVCodec Word8 ReadOnly
codec_max_lowres = Field #{offset AVCodec, max_lowres} []

getSupportedFramerates :: MonadIO m => AVCodec -> m [AVRational]
getSupportedFramerates c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, supported_framerates} pc
	if parr == nullPtr then return [] else catMaybes <$> peekArray0 Nothing parr

getSupportedPixelFormats :: MonadIO m => AVCodec -> m [AVPixelFormat]
getSupportedPixelFormats c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, pix_fmts} pc
	if parr == nullPtr then return [] else peekArray0 (toCEnum (-1)) parr

getSupportedSamplerates :: MonadIO m => AVCodec -> m [CInt]
getSupportedSamplerates c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, supported_samplerates} pc
	if parr == nullPtr then return [] else peekArray0 0 parr

getSupportedSampleFormats :: MonadIO m => AVCodec -> m [AVSampleFormat]
getSupportedSampleFormats c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, sample_fmts} pc
	if parr == nullPtr then return [] else peekArray0 (toCEnum (-1)) parr

getSupportedChannelLayouts :: MonadIO m => AVCodec -> m [AVChannelLayout]
getSupportedChannelLayouts c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, channel_layouts} pc
	if parr == nullPtr then return [] else peekArray0 (AVChannelLayout 0) parr

getSupportedProfiles :: MonadIO m => AVCodec -> m [FFProfile]
getSupportedProfiles c = liftIO.withThis c$ \pc -> do
	parr <- #{peek AVCodec, profiles} pc
	if parr == nullPtr then return [] else peekArray0 FFProfileUnknown parr

getCodecClass :: MonadIO m => AVCodec -> m (AVClass AVCodec)
getCodecClass c = liftIO.withThis c$ \pc -> do
	AVClass <$> #{peek AVCodec, priv_class} pc

