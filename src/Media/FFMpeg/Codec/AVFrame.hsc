{-# LANGUAGE ForeignFunctionInterface #-}

{- |

Description : AVFrame and related functions
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Enumerations for libavcodec.

-}

module Media.FFMpeg.Codec.AVFrame (
	AVFrame,
	AVFrameSideData,
	frameGetBestEffortTimestamp,
	frameSetBestEffortTimestamp,
	frameGetPktDuration,
	frameSetPktDuration,
	frameGetPktPos,
	frameSetPktPos,
	frameGetChannelLayout,
	frameSetChannelLayout,
	frameGetChannels,
	frameSetChannels,
	frameGetSampleRate,
	frameSetSampleRate,
	frameGetMetadata,
	frameSetMetadata,
	frameGetDecodeErrorFlags,
	frameSetDecodeErrorFlags,
	frameGetPktSize,
	frameSetPktSize,
	frameGetColorspace,
	frameSetColorspace,
	frameGetColorRange,
	frameSetColorRange
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad.IO.Class
import Data.Int
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums

newtype AVFrame = AVFrame (ForeignPtr AVFrame)
newtype AVFrameSideData = AVFrameSideData (ForeignPtr AVFrameSideData)

instance ExternalPointer AVFrame where
	withThis (AVFrame f) io = withForeignPtr f (io . castPtr)

instance ExternalPointer AVFrameSideData where
	withThis (AVFrameSideData f) io = withForeignPtr f (io . castPtr)

foreign import ccall "av_frame_get_best_effort_timestamp" av_frame_get_best_effort_timestamp :: Ptr () -> IO Int64
foreign import ccall "av_frame_set_best_effort_timestamp" av_frame_set_best_effort_timestamp :: Ptr () -> Int64 -> IO ()
foreign import ccall "av_frame_get_pkt_duration" av_frame_get_pkt_duration :: Ptr () -> IO Int64
foreign import ccall "av_frame_set_pkt_duration" av_frame_set_pkt_duration :: Ptr () -> Int64 -> IO ()
foreign import ccall "av_frame_get_pkt_pos" av_frame_get_pkt_pos :: Ptr () -> IO Int64
foreign import ccall "av_frame_set_pkt_pos" av_frame_set_pkt_pos :: Ptr () -> Int64 -> IO ()
foreign import ccall "av_frame_get_channel_layout" av_frame_get_channel_layout :: Ptr () -> IO Int64
foreign import ccall "av_frame_set_channel_layout" av_frame_set_channel_layout :: Ptr () -> Int64 -> IO ()
foreign import ccall "av_frame_get_channels" av_frame_get_channels :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_channels" av_frame_set_channels :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_get_sample_rate" av_frame_get_sample_rate :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_sample_rate" av_frame_set_sample_rate :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_get_metadata" av_frame_get_metadata :: Ptr () -> IO (Ptr ())
foreign import ccall "av_frame_set_metadata" av_frame_set_metadata :: Ptr () -> Ptr () -> IO ()
foreign import ccall "av_frame_get_decode_error_flags" av_frame_get_decode_error_flags :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_decode_error_flags" av_frame_set_decode_error_flags :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_get_pkt_size" av_frame_get_pkt_size :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_pkt_size" av_frame_set_pkt_size :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_get_qp_table" av_frame_get_qp_table :: Ptr () -> Ptr CInt -> Ptr CInt -> IO (Ptr Int8)
foreign import ccall "av_frame_set_qp_table" av_frame_set_qp_table :: Ptr () -> Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall "av_frame_get_colorspace" av_frame_get_colorspace :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_colorspace" av_frame_set_colorspace :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_get_color_range" av_frame_get_color_range :: Ptr () -> IO CInt
foreign import ccall "av_frame_set_color_range" av_frame_set_color_range :: Ptr () -> CInt -> IO ()

frameGetBestEffortTimestamp :: MonadIO m => AVFrame -> m Int64
frameGetBestEffortTimestamp frame = liftIO.withThis frame$ \ptr -> av_frame_get_best_effort_timestamp ptr
frameSetBestEffortTimestamp :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetBestEffortTimestamp frame val = liftIO.withThis frame$ \ptr -> av_frame_set_best_effort_timestamp ptr val
frameGetPktDuration :: MonadIO m => AVFrame -> m Int64
frameGetPktDuration frame = liftIO.withThis frame$ \ptr -> av_frame_get_pkt_duration ptr
frameSetPktDuration :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktDuration frame val = liftIO.withThis frame$ \ptr -> av_frame_set_pkt_duration ptr val
frameGetPktPos :: MonadIO m => AVFrame -> m Int64
frameGetPktPos frame = liftIO.withThis frame$ \ptr -> av_frame_get_pkt_pos ptr
frameSetPktPos :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktPos frame val = liftIO.withThis frame$ \ptr -> av_frame_set_pkt_pos ptr val
frameGetChannelLayout :: MonadIO m => AVFrame -> m Int64
frameGetChannelLayout frame = liftIO.withThis frame$ \ptr -> av_frame_get_channel_layout ptr
frameSetChannelLayout :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetChannelLayout frame val = liftIO.withThis frame$ \ptr -> av_frame_set_channel_layout ptr val
frameGetChannels :: MonadIO m => AVFrame -> m Int
frameGetChannels frame = liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_channels ptr
frameSetChannels :: MonadIO m => AVFrame -> Int -> m ()
frameSetChannels frame val = liftIO.withThis frame$ \ptr -> av_frame_set_channels ptr (fromIntegral val)
frameGetSampleRate :: MonadIO m => AVFrame -> m Int
frameGetSampleRate frame = liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_sample_rate ptr
frameSetSampleRate :: MonadIO m => AVFrame -> Int -> m ()
frameSetSampleRate frame val = liftIO.withThis frame$ \ptr -> av_frame_set_sample_rate ptr (fromIntegral val)
frameGetMetadata :: MonadIO m => AVFrame -> m AVDictionary
frameGetMetadata frame = do
	rptr <- liftIO.withThis frame$ \ptr -> av_frame_get_metadata ptr
	unsafeDictCopyFromPtr rptr []
frameSetMetadata :: MonadIO m => AVFrame -> AVDictionary -> m ()
frameSetMetadata frame dict = liftIO$
	withThis frame$ \ptr ->
	withThis dict$ \ppd -> do
		pd <- peek ppd
		av_frame_set_metadata ptr pd
frameGetDecodeErrorFlags :: MonadIO m => AVFrame -> m Int
frameGetDecodeErrorFlags frame = liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_decode_error_flags ptr
frameSetDecodeErrorFlags :: MonadIO m => AVFrame -> Int -> m ()
frameSetDecodeErrorFlags frame val = liftIO.withThis frame$ \ptr -> av_frame_set_decode_error_flags ptr (fromIntegral val)
frameGetPktSize :: MonadIO m => AVFrame -> m Int
frameGetPktSize frame = liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_pkt_size ptr
frameSetPktSize :: MonadIO m => AVFrame -> Int -> m ()
frameSetPktSize frame val = liftIO.withThis frame$ \ptr -> av_frame_set_pkt_size ptr (fromIntegral val)
--frameGetQpTable :: AVFrame -> Ptr CInt -> Ptr CInt -> IO (Ptr Int8)
--frameSetQpTable :: AVFrame -> Ptr () -> CInt -> CInt -> IO CInt
frameGetColorspace :: MonadIO m => AVFrame -> m AVColorSpace
frameGetColorspace frame = liftIO.withThis frame$ \ptr -> toCEnum <$> av_frame_get_colorspace ptr
frameSetColorspace :: MonadIO m => AVFrame -> AVColorSpace -> m ()
frameSetColorspace frame val = liftIO.withThis frame$ \ptr -> av_frame_set_colorspace ptr (fromCEnum val)
frameGetColorRange :: MonadIO m => AVFrame -> m AVColorRange
frameGetColorRange frame = liftIO.withThis frame$ \ptr -> toCEnum <$> av_frame_get_color_range ptr
frameSetColorRange :: MonadIO m => AVFrame -> AVColorRange -> m ()
frameSetColorRange frame val = liftIO.withThis frame$ \ptr -> av_frame_set_color_range ptr (fromCEnum val)

-- | Allocate an AVFrame
{-
allocAVFrame :: (MonadIO m, MonadError String m) =>
		PixelFormat      -- ^ pixel format of the image
		-> Int           -- ^ width
		-> Int           -- ^ height
		-> m AVFrame
allocAVFrame pf w h = do
	pFrame <- liftIO$ avcodec_frame_alloc
	if (pFrame == nullPtr) then do
		throwError "allocAVFrame: failed to allocate AVFrame"
	else do
		r <- liftIO$ avpicture_fill
				(castPtr pFrame)
				nullPtr
				(fromCEnum pf)
				(fromIntegral w) (fromIntegral h)
		if r < 0 then
			throwError$ "allocAVFrame: failed to fill picture information, error code "
				++ (show r)
		else do
			r <- liftIO$ av_frame_get_buffer (castPtr pFrame) 8  -- TODO: determine the correct alignment value
			if r < 0 then
				throwError$ "allocAVFrame: failed to allocate buffers, error code "
					++ (show r)
			else liftIO$
				(AVFrame . castForeignPtr) <$> newForeignPtr pavcodec_frame_free pFrame
-}

