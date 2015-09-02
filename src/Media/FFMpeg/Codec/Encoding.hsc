{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Encoding (
	findEncoder,
	findEncoderByName,
	encodeAudio,
	encodeVideo,

	flushAudioEnc,
	flushVideoEnc
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Ratio
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Codec.AVPacket
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Codec.Fields
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "avcodec_find_encoder" avcodec_find_encoder :: CInt -> IO (Ptr AVCodec)
foreign import ccall "avcodec_find_encoder_by_name" avcodec_find_encoder_by_name :: CString -> IO (Ptr AVCodec)
foreign import ccall "avcodec_encode_audio2" avcodec_encode_audio2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
foreign import ccall "avcodec_encode_video2" avcodec_encode_video2 :: Ptr AVCodecContext -> Ptr AVPacket -> Ptr AVFrame -> Ptr CInt -> IO CInt
foreign import ccall "avcodec_encode_subtitle" avcodec_encode_subtitle :: Ptr AVCodecContext -> Ptr Word8 -> CInt -> Ptr () -> IO CInt
 
-- | Find an encoder for a given AVCodecID
findEncoder :: MonadIO m => AVCodecID -> m (Maybe AVCodec)
findEncoder cid = liftIO$ do
	r <- avcodec_find_encoder (fromCEnum cid)
	if r == nullPtr then return Nothing else return.Just$ (AVCodec r)

-- | Find the encoder with the given name
findEncoderByName :: MonadIO m => String -> m (Maybe AVCodec)
findEncoderByName s = liftIO.withCString s$ \ps -> do
	r <- avcodec_find_encoder_by_name ps
	if r == nullPtr then return Nothing else return.Just$ (AVCodec r)

-- | Encode an audio frame.  If the AVPacket contains any data, it will be
-- freed.  If the encoder produces output, a new buffer will be allocated in
-- the AVPacket.
encodeAudio :: (MonadIO m, MonadError String m) =>
	AVCodecContext   -- ^ The codec context
	-> AVPacket      -- ^ The packet that will contain the output buffer
	-> AVFrame       -- ^ The frame to encode
	-> m Bool        -- ^ True if the encoder produced output, false otherwise
encodeAudio ctx pkt frame = do
	packetFree pkt

	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_encode_audio2 pctx ppkt pframe pGotFrame
			g <- peek pGotFrame
			return (r, g)

	when (r /= 0)$
		throwError$ "encodeAudio: avcodec_encode_audio2 failed with error code " ++ (show r)

	return (g == 1)

-- | Flush a delayed audio codec.  This function should be called repeatedly
-- until it returns False.
flushAudioEnc :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVPacket -> m Bool
flushAudioEnc ctx pkt = do
	packetFree pkt

	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_encode_audio2 pctx ppkt nullPtr pGotFrame
			g <- peek pGotFrame
			return (r, g)

	when (r /= 0)$
		throwError$ "flushAudioEnc: avcodec_encode_audio2 failed with error code " ++ (show r)

	return$ g == 1

-- | Encode a video frame.  If the AVPacket contains any data, it will be
-- freed.  If the encoder produces output, a new buffer will be allocated in
-- the AVPacket.
encodeVideo :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVPacket -> AVFrame -> m Bool
encodeVideo ctx pkt frame = do
	packetFree pkt

	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_encode_video2 pctx ppkt pframe pGotFrame
			g <- peek pGotFrame
			return (r, g)

	when (r /= 0)$
		throwError$ "encodeVideo: avcodec_encode_video2 failed with error code " ++ (show r)

	return (g == 1)

-- | Flush a delayed video codec.  This function should be called repeatedly
-- until it returns False.
flushVideoEnc :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVPacket -> m Bool
flushVideoEnc ctx pkt = do
	packetFree pkt

	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_encode_video2 pctx ppkt nullPtr pGotFrame
			g <- peek pGotFrame
			return (r, g)

	when (r /= 0)$
		throwError$ "flushVideoEnc: avcodec_encode_video2 failed with error code " ++ (show r)

	return$ g == 1

-- | Encode a subtitle.  This function takes two AVPackets because some
-- subtitles encode into more than one packet.  The return value contains the
-- number of packets used (which will be 0, 1, or 2).
encodeSubtitle :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVPacket -> AVPacket -> AVSubtitle -> m Int
encodeSubtitle ctx pkt1 pkt2 subtitle = do
	-- This procedure is based on "do_subtitle_out()" from "ffmpeg.c" (link:
	-- https://www.ffmpeg.org/doxygen/trunk/ffmpeg_8c_source.html#l00857)

	-- marshal the subtitle
	psub <- liftIO.(fmap castPtr).av_malloc.fromIntegral$ sizeOf (undefined :: AVSubtitle)
	liftIO$ poke psub subtitle

	mpktTimebase <- codecGetPktTimebase ctx
	pktTimebase <- case mpktTimebase of
		Just x -> return x
		Nothing -> throwError$ "encodeSubtitle: packet time base invalid"

	codecID <- (return.avCodecDescriptor_id) =<< codecGetCodecDescriptor ctx

	-- preadjust the timing
	let newPts = (avSubtitle_pts subtitle) + (rescaleTSQ
		(fromIntegral$ avSubtitle_start_display_time subtitle)
		(AVRational$ 1 % 1000) avTimeBaseQ)
	let newEndDisplayTime =
		(avSubtitle_end_display_time subtitle) - (avSubtitle_start_display_time subtitle)
	
	liftIO$ #{poke AVSubtitle, pts} psub newPts
	liftIO$ #{poke AVSubtitle, start_display_time} psub (0 :: Word32)
	liftIO$ #{poke AVSubtitle, end_display_time} psub newEndDisplayTime

	-- write the first packet
	allocBufferRef subtitleOutMaxSize$ \pavbuff -> do
		(pbuff, size) <- getBufferData pavbuff
		r <- liftIO.withThis ctx$ \pctx ->
			avcodec_encode_subtitle pctx pbuff (fromIntegral size) (castPtr psub)
		when (r /= 0)$ do
			liftIO$ avsubtitle_free (castPtr psub)
			liftIO$ with pavbuff av_buffer_unref
			throwError$ "encodeSubtitle: 1st avcodec_encode_subtitle failed with error code " ++ (show r)

		newPacketFromBuffer pkt1 pavbuff
		setField packet_pts pkt1$ rescaleTSQ newPts avTimeBaseQ pktTimebase
		setField packet_dts pkt1$ rescaleTSQ newPts avTimeBaseQ pktTimebase
		setField packet_duration pkt1.fromIntegral$
			rescaleQ (fromIntegral newEndDisplayTime) (AVRational$ 1 % 1000) pktTimebase
	
	-- write the extra packet for DVB subtitles
	when (codecID == AVCodecIdDvbSubtitle)$ do
		allocBufferRef subtitleOutMaxSize$ \pavbuff -> do
			(pbuff, size) <- getBufferData pavbuff
			nrects <- liftIO (#{peek AVSubtitle, num_rects} psub :: IO CUInt)
			liftIO$ #{poke AVSubtitle, num_rects} psub (0 :: CUInt)
			r <- liftIO.withThis ctx$ \pctx ->
				avcodec_encode_subtitle pctx pbuff (fromIntegral size) (castPtr psub)
			liftIO$ #{poke AVSubtitle, num_rects} psub nrects
			when (r /= 0)$ do
				liftIO$ avsubtitle_free (castPtr psub)
				liftIO$ with pavbuff av_buffer_unref
				packetFree pkt1
				throwError$ "encodeSubtitle: 2nd avcodec_encode_subtitle failed with error code " ++ (show r)

			newPacketFromBuffer pkt2 pavbuff
			setField packet_pts pkt2$
				(rescaleTSQ newPts avTimeBaseQ pktTimebase) +
				(AVTimestamp$ fromIntegral (90 * newEndDisplayTime))
			setField packet_dts pkt2$ rescaleTSQ newPts avTimeBaseQ pktTimebase
			setField packet_duration pkt2$ fromIntegral$
				rescaleQ (fromIntegral newEndDisplayTime) (AVRational$ 1 % 1000) pktTimebase

	if (codecID == AVCodecIdDvbSubtitle) then return 2 else return 1

	where
		subtitleOutMaxSize = 1024 * 1024

