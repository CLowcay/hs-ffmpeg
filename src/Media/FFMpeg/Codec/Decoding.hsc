{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Decoding (
	findDecoder,
	findDecoderByName,
	alignDimensions,
	alignDimensions2,
	toChromaPos,
	fromChromaPos,
	decodeAudio,
	decodeVideo,
	decodeSubtitle,

	flushAudioDec,
	flushVideoDec,
	flushSubtitleDec
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec.AVPacket
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "avcodec_find_decoder" avcodec_find_decoder :: CInt -> IO (Ptr AVCodec)
foreign import ccall "avcodec_find_decoder_by_name" avcodec_find_decoder_by_name :: CString -> IO (Ptr AVCodec)
foreign import ccall "avcodec_default_get_buffer2" avcodec_default_get_buffer2 :: Ptr AVCodecContext -> Ptr AVFrame -> CInt -> IO CInt
foreign import ccall "avcodec_align_dimensions" avcodec_align_dimensions :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "avcodec_align_dimensions2" avcodec_align_dimensions2 :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "avcodec_enum_to_chroma_pos" avcodec_enum_to_chroma_pos :: Ptr CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall "avcodec_chroma_pos_to_enum" avcodec_chroma_pos_to_enum :: CInt -> CInt -> CInt
foreign import ccall "avcodec_decode_audio4" avcodec_decode_audio4 :: Ptr AVCodecContext -> Ptr AVFrame -> Ptr CInt -> Ptr AVPacket -> IO CInt
foreign import ccall "avcodec_decode_video2" avcodec_decode_video2 :: Ptr AVCodecContext -> Ptr AVFrame -> Ptr CInt -> Ptr AVPacket -> IO CInt
foreign import ccall "avcodec_decode_subtitle2" avcodec_decode_subtitle2 :: Ptr AVCodecContext -> Ptr () -> Ptr CInt -> Ptr AVPacket -> IO CInt

-- Find a decoder for a given AVCodecID
findDecoder :: MonadIO m => AVCodecID -> m (Maybe AVCodec)
findDecoder cid = liftIO$
	(fmap AVCodec).justPtr <$> avcodec_find_decoder (fromCEnum cid)

-- Find the decoder with the given name
findDecoderByName :: MonadIO m => String -> m (Maybe AVCodec)
findDecoderByName s = liftIO.withCString s$ \ps ->
	(fmap AVCodec).justPtr <$> avcodec_find_decoder_by_name ps

-- | Adjust dimensions (width, height) to something suitable for the decoder
alignDimensions :: MonadIO m => AVCodecContext -> (Int, Int) -> m (Int, Int)
alignDimensions ctx (w, h) = liftIO.withThis ctx$ \ptr ->
	alloca$ \pw ->
	alloca$ \ph -> do
		poke pw (fromIntegral w)
		poke ph (fromIntegral h)
		avcodec_align_dimensions ptr pw ph
		w' <- fromIntegral <$> peek pw
		h' <- fromIntegral <$> peek ph
		return (w', h')

-- | Adjust dimensions (width, height) to something suitable for the decoder,
-- ensuring all the lines are multiples of the given line alignment values
alignDimensions2 :: MonadIO m =>
	AVCodecContext   -- ^ Codec context
	-> (Int, Int)    -- ^ (width, height)
	-> [Int]         -- ^ Line alignments
	-> m (Int, Int, [Int])
alignDimensions2 ctx (w, h) ls = liftIO.withThis ctx$ \ptr ->
	alloca$ \pw ->
	alloca$ \ph ->
	withArray (take AVNumDataPointers$ (fromIntegral <$> ls) ++ [1..]) $ \pls -> do
		poke pw (fromIntegral w)
		poke ph (fromIntegral h)
		avcodec_align_dimensions2 ptr pw ph pls
		w' <- fromIntegral <$> peek pw
		h' <- fromIntegral <$> peek ph
		ls' <- (fmap fromIntegral) <$> peekArray AVNumDataPointers pls
		return (w', h', ls')

-- | Get the (x, y) coordinates of an AVChromaLocation
toChromaPos :: AVChromaLocation -> (Int, Int)
toChromaPos cl = unsafePerformIO$  -- safe because avcodec_enum_to_chroma_pos has no observable side effects
	alloca$ \px ->
	alloca$ \py -> do
		avcodec_enum_to_chroma_pos px py (fromCEnum cl)
		x' <- fromIntegral <$> peek px
		y' <- fromIntegral <$> peek py
		return (x', y')

-- | Convert (x, y) coordinates to an AVChromaLocation
fromChromaPos :: (Int, Int) -> AVChromaLocation
fromChromaPos (x, y) =
	toCEnum$ avcodec_chroma_pos_to_enum (fromIntegral x) (fromIntegral y)

-- | Decode a single frame of audio.  If the AVFrame already contains decoded
-- data, then it will be replaced and the old data freed.  Note that it may be
-- necessary to call this function multiple times to decode all the audio in a
-- packet.
decodeAudio :: (MonadIO m, MonadError String m) =>
	AVCodecContext   -- ^ Codec context
	-> AVFrame       -- ^ Frame to store the decoded audio
	-> AVPacket      -- ^ Packet containing the input buffer
	-> m (Bool, Int) -- ^ (did decode frame, bytes consumed)
decodeAudio ctx frame pkt = do
	frameUnref frame
	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_audio4 pctx pframe pGotFrame ppkt
			g <- peek pGotFrame
			return (r, g)
	
	when (r < 0)$
		throwError$ "decodeAudio: avcodec_decode_audio4 failed with error code " ++ (show r)
	
	return (g /= 0, fromIntegral r)

-- | Flush a delayed audio codec.  Returns True if output was produced.  This
-- function should be called repeatedly until it returns False.
flushAudioDec :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVFrame -> m Bool
flushAudioDec ctx frame = do
	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_audio4 pctx pframe pGotFrame nullPtr
			g <- peek pGotFrame
			return (r, g)

	when (r < 0)$
		throwError$ "flushAudioDec: avcodec_decode_audio4 failed with error code " ++ (show r)
	
	return$ g /= 0

-- | Decode a single frame of video.  If the AVFrame already contains decoded
-- data, then it will be replaced and the old data freed.  Generally a single
-- packet will contain a single frame, but this is apparently not always the
-- case, so it may be necessary to call this function multiple times to decode
-- a single packet.
decodeVideo :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVFrame -> AVPacket -> m (Bool, Int)
decodeVideo ctx frame pkt = do
	frameUnref frame
	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		withThis pkt$ \ppkt ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_video2 pctx pframe pGotFrame ppkt
			g <- peek pGotFrame
			return (r, g)
	
	when (r < 0)$
		throwError$ "decodeVideo: avcodec_decode_video2 failed with error code " ++ (show r)
	
	return (g /= 0, fromIntegral r)

-- | Flush a delayed video codec.  Returns True if output was produced.  This
-- function should be called repeatedly until it returns False.
flushVideoDec :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVFrame -> m Bool
flushVideoDec ctx frame = do
	(r, g) <- liftIO$
		withThis ctx$ \pctx ->
		withThis frame$ \pframe ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_video2 pctx pframe pGotFrame nullPtr
			g <- peek pGotFrame
			return (r, g)

	when (r < 0)$
		throwError$ "flushVideoDec: avcodec_decode_video4 failed with error code " ++ (show r)
	
	return$ g /= 0

-- | Decode a subtitle.  As for Video, one packet will generally contain one
-- subtitle, but the API doesn't seem to guarantee it, so it may be necessary
-- to call this function multiple times to decode a single packet.
decodeSubtitle :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> AVPacket -> m (Maybe AVSubtitle, Int)
decodeSubtitle ctx pkt = do
	(r, s) <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt ->
		alloca$ \psub ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_subtitle2 pctx (castPtr psub) pGotFrame ppkt
			g <- peek pGotFrame
			s <- if g /= 0 then Just <$> peek psub else return Nothing
			return (r, s)
	
	when (r < 0)$
		throwError$ "decodeSubtitle: avcodec_decode_subtitle failed with error code " ++ (show r)
	
	return (s, fromIntegral r)

-- | Flush a delayed subtitle codec.  This function should be called until it
-- returns Nothing.
flushSubtitleDec :: (MonadIO m, MonadError String m) =>
	AVCodecContext -> m (Maybe AVSubtitle)
flushSubtitleDec ctx = do
	(r, s) <- liftIO$
		withThis ctx$ \pctx ->
		alloca$ \psub ->
		alloca$ \pGotFrame -> do
			r <- avcodec_decode_subtitle2 pctx (castPtr psub) pGotFrame nullPtr
			g <- peek pGotFrame
			s <- if g /= 0 then Just <$> peek psub else return Nothing
			return (r, s)
	
	when (r < 0)$
		throwError$ "flushSubtitleDec: avcodec_decode_subtitle failed with error code " ++ (show r)
	
	return s

