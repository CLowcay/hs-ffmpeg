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

import Control.Monad
import Control.Monad.Except
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Codec.AVPacket
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrame

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

