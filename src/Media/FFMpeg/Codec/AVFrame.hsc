{-# LANGUAGE FlexibleContexts #-}
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
	frameSetColorRange,

	getColorspaceName,
	frameAlloc,
	frameRef,
	frameClone,
	frameUnref,
	frameMoveRef,
	frameGetBuffer,
	frameIsWritable,
	frameMakeWritable,
	frameCopyProps,
	frameNewSideData,
	withFrameSideData,
	frameRemoveSideData,
	frameSideDataName
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums

newtype AVFrame = AVFrame (ForeignPtr AVFrame)
newtype AVFrameSideData = AVFrameSideData (Ptr AVFrameSideData)

instance ExternalPointer AVFrame where
	withThis (AVFrame f) io = withForeignPtr f (io.castPtr)

instance ExternalPointer AVFrameSideData where
	withThis (AVFrameSideData f) io = io.castPtr$ f

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

foreign import ccall "av_get_colorspace_name" av_get_colorspace_name :: CInt -> CString
foreign import ccall "avcodec_frame_alloc" avcodec_frame_alloc :: IO (Ptr ())
foreign import ccall "avcodec_frame_free" avcodec_frame_free :: Ptr () -> IO ()
foreign import ccall "&avcodec_frame_free" pavcodec_frame_free :: FunPtr (Ptr () -> IO ())

foreign import ccall "av_frame_ref" av_frame_ref :: Ptr () -> Ptr() -> IO CInt
foreign import ccall "av_frame_clone" av_frame_clone :: Ptr () -> IO (Ptr())
foreign import ccall "av_frame_unref" av_frame_unref :: Ptr () -> IO ()
foreign import ccall "av_frame_move_ref" av_frame_move_ref :: Ptr () -> Ptr() -> IO ()
foreign import ccall "av_frame_get_buffer" av_frame_get_buffer :: Ptr () -> CInt -> IO CInt
foreign import ccall "av_frame_is_writable" av_frame_is_writable :: Ptr () -> IO CInt
foreign import ccall "av_frame_make_writable" av_frame_make_writable :: Ptr () -> IO CInt
foreign import ccall "av_frame_copy" av_frame_copy :: Ptr () -> Ptr() -> IO CInt
foreign import ccall "av_frame_copy_props" av_frame_copy_props :: Ptr () -> Ptr () -> IO CInt
foreign import ccall "av_frame_get_plane_buffer" av_frame_get_plane_buffer :: Ptr () -> CInt -> Ptr ()
foreign import ccall "av_frame_new_side_data" av_frame_new_side_data :: Ptr () -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "av_frame_get_side_data" av_frame_get_side_data :: Ptr () -> CInt -> IO (Ptr ())
foreign import ccall "av_frame_remove_side_data" av_frame_remove_side_data :: Ptr () -> CInt -> IO ()
foreign import ccall "av_frame_side_data_name" av_frame_side_data_name :: CInt -> CString

frameGetBestEffortTimestamp :: MonadIO m => AVFrame -> m Int64
frameGetBestEffortTimestamp frame =
	liftIO.withThis frame$ \ptr -> av_frame_get_best_effort_timestamp ptr

frameSetBestEffortTimestamp :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetBestEffortTimestamp frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_best_effort_timestamp ptr val

frameGetPktDuration :: MonadIO m => AVFrame -> m Int64
frameGetPktDuration frame =
	liftIO.withThis frame$ \ptr -> av_frame_get_pkt_duration ptr

frameSetPktDuration :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktDuration frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_pkt_duration ptr val

frameGetPktPos :: MonadIO m => AVFrame -> m Int64
frameGetPktPos frame =
	liftIO.withThis frame$ \ptr -> av_frame_get_pkt_pos ptr

frameSetPktPos :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktPos frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_pkt_pos ptr val

frameGetChannelLayout :: MonadIO m => AVFrame -> m Int64
frameGetChannelLayout frame =
	liftIO.withThis frame$ \ptr -> av_frame_get_channel_layout ptr

frameSetChannelLayout :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetChannelLayout frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_channel_layout ptr val

frameGetChannels :: MonadIO m => AVFrame -> m Int
frameGetChannels frame =
	liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_channels ptr

frameSetChannels :: MonadIO m => AVFrame -> Int -> m ()
frameSetChannels frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_channels ptr (fromIntegral val)

frameGetSampleRate :: MonadIO m => AVFrame -> m Int
frameGetSampleRate frame =
	liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_sample_rate ptr

frameSetSampleRate :: MonadIO m => AVFrame -> Int -> m ()
frameSetSampleRate frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_sample_rate ptr (fromIntegral val)

-- | Get a /copy/ of the metadata associated with an AVFrame.  This differs
-- from the raw function, which does not copy the data.
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
frameGetDecodeErrorFlags frame =
	liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_decode_error_flags ptr

frameSetDecodeErrorFlags :: MonadIO m => AVFrame -> Int -> m ()
frameSetDecodeErrorFlags frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_decode_error_flags ptr (fromIntegral val)

frameGetPktSize :: MonadIO m => AVFrame -> m Int
frameGetPktSize frame =
	liftIO.withThis frame$ \ptr -> fromIntegral <$> av_frame_get_pkt_size ptr

frameSetPktSize :: MonadIO m => AVFrame -> Int -> m ()
frameSetPktSize frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_pkt_size ptr (fromIntegral val)

--frameGetQpTable :: AVFrame -> Ptr CInt -> Ptr CInt -> IO (Ptr Int8)
--frameSetQpTable :: AVFrame -> Ptr () -> CInt -> CInt -> IO CInt

frameGetColorspace :: MonadIO m => AVFrame -> m AVColorSpace
frameGetColorspace frame =
	liftIO.withThis frame$ \ptr -> toCEnum <$> av_frame_get_colorspace ptr

frameSetColorspace :: MonadIO m => AVFrame -> AVColorSpace -> m ()
frameSetColorspace frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_colorspace ptr (fromCEnum val)

frameGetColorRange :: MonadIO m => AVFrame -> m AVColorRange
frameGetColorRange frame =
	liftIO.withThis frame$ \ptr -> toCEnum <$> av_frame_get_color_range ptr

frameSetColorRange :: MonadIO m => AVFrame -> AVColorRange -> m ()
frameSetColorRange frame val =
	liftIO.withThis frame$ \ptr -> av_frame_set_color_range ptr (fromCEnum val)

-- | Get a String representation of an AVColorSpace
getColorspaceName :: AVColorSpace -> String
getColorspaceName space =
	-- safe because the c string is immutable
	unsafePerformIO.peekCString.av_get_colorspace_name$ fromCEnum space

-- | Allocate a new AVFrame.  The AVFrame is uninitialised and no buffers are
-- allocated.
frameAlloc :: (MonadIO m, MonadError String m) => m AVFrame
frameAlloc = do
	pFrame <- liftIO$ avcodec_frame_alloc
	if (pFrame == nullPtr) then do
		throwError "allocAVFrame: failed to allocate AVFrame"
	else liftIO$
		(AVFrame . castForeignPtr) <$> newForeignPtr pavcodec_frame_free pFrame

-- | Copy the data from one AVFrame to another.  If the frame is reference
-- counted, then a new reference is created, otherwise the data is actually
-- copied
frameRef :: (MonadIO m, MonadError String m) =>
	AVFrame          -- ^ destination frame
	-> AVFrame       -- ^ source frame
	-> m ()
frameRef dst src = do
	r <- liftIO$
		withThis dst$ \pd ->
		withThis src$ \ps -> av_frame_ref pd ps
	when (r /= 0)$
		throwError$ "frameRef: failed with error code " ++ (show r)

-- | Create a new copy of an AVFrame .  If the frame is reference counted, then
-- the new frame references the same data as the old one, otherwise the buffers
-- are copied into the new frame.
frameClone :: (MonadIO m, MonadError String m) => AVFrame -> m AVFrame
frameClone src = do
	r <- liftIO.withThis src$ \ptr -> av_frame_clone ptr
	if r == nullPtr then
		throwError$ "frameClone: av_frame_clone returned a null pointer"
	else liftIO$
		(AVFrame . castForeignPtr) <$> newForeignPtr pavcodec_frame_free r

-- | Reset a frame to its default uninitialised state, unreferencing any
-- reference counted buffers.
frameUnref :: MonadIO m => AVFrame -> m ()
frameUnref frame = liftIO.withThis frame$ \ptr -> av_frame_unref ptr

-- | Shortcut for @frameRef dst src >> frameUnref src@
frameMoveRef :: MonadIO m =>
	AVFrame          -- ^ destination frame
	-> AVFrame       -- ^ source frame
	-> m ()
frameMoveRef dst src = liftIO$
	withThis dst$ \pd ->
	withThis src$ \ps -> av_frame_move_ref pd ps

-- | Allocate a buffer for an AVFrame
frameGetBuffer :: (MonadIO m, MonadError String m) =>
	AVFrame          -- ^ the AVFrame
	-> Int           -- ^ alignment for the buffer
	-> m ()
frameGetBuffer frame align = do
	r <- liftIO.withThis frame$ \ptr -> av_frame_get_buffer ptr (fromIntegral align)
	when (r /= 0)$
		throwError$ "frameGetBuffer: failed with error code " ++ (show r)

-- | Determine if a frame is writeable
frameIsWritable :: MonadIO m => AVFrame -> m Bool
frameIsWritable frame = liftIO.withThis frame$ \ptr -> (> 0) <$> av_frame_is_writable ptr

-- | Ensure that the frame is writeable by copying the buffers if necessary.
frameMakeWritable :: (MonadIO m, MonadError String m) => AVFrame -> m ()
frameMakeWritable frame = do
	r <- liftIO.withThis frame$ \ptr -> av_frame_make_writable ptr
	when (r /= 0)$
		throwError$ "frameMakeWritable: failed with error code " ++ (show r)

-- | Copy the metadata from one AVFrame to another.  Does not copy or reference
-- the buffers or the buffer layout fields.
frameCopyProps :: MonadIO m =>
	AVFrame          -- ^ destination frame
	-> AVFrame       -- ^ source frame
	-> m ()
frameCopyProps dst src = liftIO$
	withThis dst$ \pd ->
	withThis src$ \ps -> do
		av_frame_copy_props pd ps
		return ()

-- | Allocate new side side data in an AVFrame.  The AVFrameSideData is managed
-- by libav, so we need to be careful how we use it.  Hence, this function does
-- not return the newly allocated AVFrameSideData.  Use 'withFrameSideData' to
-- access the AVFrameSideData.
frameNewSideData :: (MonadIO m, MonadError String m) =>
	AVFrame                  -- ^ the frame to allocate side data in
	-> AVFrameSideDataType   -- ^ the type of the side data
	-> Int                   -- ^ the size of the side data in bytes
	-> m ()
frameNewSideData frame sdtype size = do
	r <- liftIO.withThis frame$ \ptr ->
		av_frame_new_side_data ptr (fromCEnum sdtype) (fromIntegral size)
	when (r == nullPtr)$
		throwError$ "frameNewSizeData: av_frame_new_side_data returned a null pointer"

-- | Execute a monadic action that depends on the AVFrameSideData of an
-- AVFrame.  The monadic action should not invoke 'frameRemoveSideData' or
-- 'frameUnref'
withFrameSideData :: (MonadIO m, MonadError String m) =>
	AVFrame -> AVFrameSideDataType -> (AVFrameSideData -> m a) -> m a
withFrameSideData frame sdtype action = do
	sd <- liftIO.withThis frame$ \ptr ->
		av_frame_get_side_data ptr (fromCEnum sdtype)
	if sd == nullPtr then
		throwError$ "withFrameSideData: av_frame_get_side_data returned a null pointer"
	else
		action.(AVFrameSideData).castPtr$ sd

-- | Free side data
frameRemoveSideData :: MonadIO m => AVFrame -> AVFrameSideDataType -> m ()
frameRemoveSideData frame sdtype = liftIO.withThis frame$ \ptr -> av_frame_remove_side_data ptr (fromCEnum sdtype)

-- | Get a String representation of an AVFrameSideDataType
frameSideDataName :: AVFrameSideDataType -> String
frameSideDataName sdType =
	-- safe because the c string is immutable
	unsafePerformIO.peekCString.av_frame_side_data_name$ fromCEnum sdType

