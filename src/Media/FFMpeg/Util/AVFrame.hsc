{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |

Description : AVFrame and related functions
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.AVFrame (
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

	AVFrameSideDataPayload,

	getColorspaceName,
	getPictureTypeChar,
	frameAlloc,
	frameRef,
	frameClone,
	frameUnref,
	frameMoveRef,
	frameGetBuffer,
	frameIsWritable,
	frameMakeWritable,
	frameCopyProps,
	frameSetSideData,
	frameGetSideData,
	frameSideDataName
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import System.IO.Unsafe

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrameSideData
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums
import Media.FFMpeg.Util.Options

foreign import ccall "av_frame_get_best_effort_timestamp" av_frame_get_best_effort_timestamp :: Ptr AVFrame -> IO Int64
foreign import ccall "av_frame_set_best_effort_timestamp" av_frame_set_best_effort_timestamp :: Ptr AVFrame -> Int64 -> IO ()
foreign import ccall "av_frame_get_pkt_duration" av_frame_get_pkt_duration :: Ptr AVFrame -> IO Int64
foreign import ccall "av_frame_set_pkt_duration" av_frame_set_pkt_duration :: Ptr AVFrame -> Int64 -> IO ()
foreign import ccall "av_frame_get_pkt_pos" av_frame_get_pkt_pos :: Ptr AVFrame -> IO Int64
foreign import ccall "av_frame_set_pkt_pos" av_frame_set_pkt_pos :: Ptr AVFrame -> Int64 -> IO ()
foreign import ccall "av_frame_get_channel_layout" av_frame_get_channel_layout :: Ptr AVFrame -> IO Int64
foreign import ccall "av_frame_set_channel_layout" av_frame_set_channel_layout :: Ptr AVFrame -> Int64 -> IO ()
foreign import ccall "av_frame_get_channels" av_frame_get_channels :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_channels" av_frame_set_channels :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_get_sample_rate" av_frame_get_sample_rate :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_sample_rate" av_frame_set_sample_rate :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_get_metadata" av_frame_get_metadata :: Ptr AVFrame -> IO (Ptr ())
foreign import ccall "av_frame_set_metadata" av_frame_set_metadata :: Ptr AVFrame -> Ptr () -> IO ()
foreign import ccall "av_frame_get_decode_error_flags" av_frame_get_decode_error_flags :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_decode_error_flags" av_frame_set_decode_error_flags :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_get_pkt_size" av_frame_get_pkt_size :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_pkt_size" av_frame_set_pkt_size :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_get_qp_table" av_frame_get_qp_table :: Ptr AVFrame -> Ptr CInt -> Ptr CInt -> IO (Ptr Int8)
foreign import ccall "av_frame_set_qp_table" av_frame_set_qp_table :: Ptr AVFrame -> Ptr () -> CInt -> CInt -> IO CInt
foreign import ccall "av_frame_get_colorspace" av_frame_get_colorspace :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_colorspace" av_frame_set_colorspace :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_get_color_range" av_frame_get_color_range :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_set_color_range" av_frame_set_color_range :: Ptr AVFrame -> CInt -> IO ()

foreign import ccall "av_get_colorspace_name" av_get_colorspace_name :: CInt -> CString
foreign import ccall "av_get_picture_type_char" av_get_picture_type_char :: CInt -> CChar
foreign import ccall "avcodec_frame_alloc" avcodec_frame_alloc :: IO (Ptr ())
foreign import ccall "avcodec_frame_free" avcodec_frame_free :: Ptr () -> IO ()
foreign import ccall "&avcodec_frame_free" pavcodec_frame_free :: FunPtr (Ptr () -> IO ())

foreign import ccall "av_frame_ref" av_frame_ref :: Ptr AVFrame -> Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_clone" av_frame_clone :: Ptr AVFrame -> IO (Ptr())
foreign import ccall "av_frame_unref" av_frame_unref :: Ptr AVFrame -> IO ()
foreign import ccall "av_frame_move_ref" av_frame_move_ref :: Ptr AVFrame -> Ptr AVFrame -> IO ()
foreign import ccall "av_frame_get_buffer" av_frame_get_buffer :: Ptr AVFrame -> CInt -> IO CInt
foreign import ccall "av_frame_is_writable" av_frame_is_writable :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_make_writable" av_frame_make_writable :: Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_copy" av_frame_copy :: Ptr AVFrame -> Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_copy_props" av_frame_copy_props :: Ptr AVFrame -> Ptr AVFrame -> IO CInt
foreign import ccall "av_frame_get_plane_buffer" av_frame_get_plane_buffer :: Ptr AVFrame -> CInt -> Ptr ()
foreign import ccall "av_frame_new_side_data" av_frame_new_side_data :: Ptr AVFrame -> CInt -> CInt -> IO (Ptr ())
foreign import ccall "av_frame_get_side_data" av_frame_get_side_data :: Ptr AVFrame -> CInt -> IO (Ptr ())
foreign import ccall "av_frame_remove_side_data" av_frame_remove_side_data :: Ptr AVFrame -> CInt -> IO ()
foreign import ccall "av_frame_side_data_name" av_frame_side_data_name :: CInt -> CString

foreign import ccall "avcodec_get_frame_class" avcodec_get_frame_class :: Ptr (AVClass AVFrame)

-- | AVFrame struct
newtype AVFrame = AVFrame (ForeignPtr AVFrame)
instance ExternalPointer AVFrame where
	type UnderlyingType AVFrame = AVFrame
	withThis (AVFrame f) = withThis f
instance HasClass AVFrame where
	getClass = avClassFromPtr avcodec_get_frame_class

frameGetBestEffortTimestamp :: MonadIO m => AVFrame -> m Int64
frameGetBestEffortTimestamp frame =
	withThis frame$ \ptr -> liftIO$ av_frame_get_best_effort_timestamp ptr

frameSetBestEffortTimestamp :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetBestEffortTimestamp frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_best_effort_timestamp ptr val

frameGetPktDuration :: MonadIO m => AVFrame -> m Int64
frameGetPktDuration frame =
	withThis frame$ \ptr -> liftIO$ av_frame_get_pkt_duration ptr

frameSetPktDuration :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktDuration frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_pkt_duration ptr val

frameGetPktPos :: MonadIO m => AVFrame -> m Int64
frameGetPktPos frame =
	withThis frame$ \ptr -> liftIO$ av_frame_get_pkt_pos ptr

frameSetPktPos :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetPktPos frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_pkt_pos ptr val

frameGetChannelLayout :: MonadIO m => AVFrame -> m Int64
frameGetChannelLayout frame =
	liftIO.withThis frame$ \ptr -> liftIO$ av_frame_get_channel_layout ptr

frameSetChannelLayout :: MonadIO m => AVFrame -> Int64 -> m ()
frameSetChannelLayout frame val =
	liftIO.withThis frame$ \ptr -> liftIO$ av_frame_set_channel_layout ptr val

frameGetChannels :: MonadIO m => AVFrame -> m Int
frameGetChannels frame =
	withThis frame$ \ptr -> liftIO$ fromIntegral <$> av_frame_get_channels ptr

frameSetChannels :: MonadIO m => AVFrame -> Int -> m ()
frameSetChannels frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_channels ptr (fromIntegral val)

frameGetSampleRate :: MonadIO m => AVFrame -> m Int
frameGetSampleRate frame =
	withThis frame$ \ptr -> liftIO$ fromIntegral <$> av_frame_get_sample_rate ptr

frameSetSampleRate :: MonadIO m => AVFrame -> Int -> m ()
frameSetSampleRate frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_sample_rate ptr (fromIntegral val)

-- | Get a /copy/ of the metadata associated with an AVFrame.  This differs
-- from the raw function, which does not copy the data.
frameGetMetadata :: MonadIO m => AVFrame -> m AVDictionary
frameGetMetadata frame = do
	withThis frame$ \ptr -> do
		rptr <- liftIO$ av_frame_get_metadata ptr
		unsafeDictCopyFromPtr (castPtr rptr) []

frameSetMetadata :: MonadIO m => AVFrame -> AVDictionary -> m ()
frameSetMetadata frame dict =
	withThis frame$ \ptr ->
	withThis dict$ \ppd -> liftIO$ do
		pd <- peek ppd
		av_frame_set_metadata ptr (castPtr pd)

frameGetDecodeErrorFlags :: MonadIO m => AVFrame -> m Int
frameGetDecodeErrorFlags frame =
	withThis frame$ \ptr -> liftIO$ fromIntegral <$> av_frame_get_decode_error_flags ptr

frameSetDecodeErrorFlags :: MonadIO m => AVFrame -> Int -> m ()
frameSetDecodeErrorFlags frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_decode_error_flags ptr (fromIntegral val)

frameGetPktSize :: MonadIO m => AVFrame -> m Int
frameGetPktSize frame =
	withThis frame$ \ptr -> liftIO$ fromIntegral <$> av_frame_get_pkt_size ptr

frameSetPktSize :: MonadIO m => AVFrame -> Int -> m ()
frameSetPktSize frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_pkt_size ptr (fromIntegral val)

--frameGetQpTable :: AVFrame -> Ptr CInt -> Ptr CInt -> IO (Ptr Int8)
--frameSetQpTable :: AVFrame -> Ptr () -> CInt -> CInt -> IO CInt

frameGetColorspace :: MonadIO m => AVFrame -> m AVColorSpace
frameGetColorspace frame =
	withThis frame$ \ptr -> liftIO$ toCEnum <$> av_frame_get_colorspace ptr

frameSetColorspace :: MonadIO m => AVFrame -> AVColorSpace -> m ()
frameSetColorspace frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_colorspace ptr (fromCEnum val)

frameGetColorRange :: MonadIO m => AVFrame -> m AVColorRange
frameGetColorRange frame =
	withThis frame$ \ptr -> liftIO$ toCEnum <$> av_frame_get_color_range ptr

frameSetColorRange :: MonadIO m => AVFrame -> AVColorRange -> m ()
frameSetColorRange frame val =
	withThis frame$ \ptr -> liftIO$ av_frame_set_color_range ptr (fromCEnum val)

-- | Get a String representation of an AVColorSpace
getColorspaceName :: AVColorSpace -> String
getColorspaceName space =
	-- safe because the c string is immutable
	unsafePerformIO.peekCString.av_get_colorspace_name$ fromCEnum space

-- | Get a Char representation of a picture type
getPictureTypeChar :: AVPictureType -> Char
getPictureTypeChar t = castCCharToChar$ av_get_picture_type_char (fromCEnum t)

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
	r <-
		withThis dst$ \pd ->
		withThis src$ \ps -> liftIO$ av_frame_ref pd ps
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

-- | Class of valid AVFrameSideData types
class AVFrameSideDataPayload a where
	peekAVFrameSideDataPtr :: Ptr AVFrame -> IO (Ptr a)
	getPayloadType :: a -> AVFrameSideDataType
	getPayloadSize :: a -> CInt
	peekPayload :: Ptr a -> Int -> IO a
	pokePayload :: Ptr a -> a -> IO ()

instance AVFrameSideDataPayload AVFrameDataPanScan where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypePanscan)
	getPayloadType _ = AVFrameDataTypePanscan
	getPayloadSize (AVFrameDataPanScan x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataA53CC where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeA53CC)
	getPayloadType _ = AVFrameDataTypeA53CC
	getPayloadSize (AVFrameDataA53CC s) = fromIntegral$ B.length s
	peekPayload ptr size = AVFrameDataA53CC <$> B.packCStringLen (castPtr ptr, size)
	pokePayload ptr (AVFrameDataA53CC s) = B.unsafeUseAsCString s$ \ps ->
		copyArray (castPtr ptr) ps (B.length s)
instance AVFrameSideDataPayload AVFrameDataStereo3d where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeStereo3d)
	getPayloadType _ = AVFrameDataTypeStereo3d
	getPayloadSize (AVFrameDataStereo3d  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataMatrixEncoding where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeMatrixEncoding)
	getPayloadType _ = AVFrameDataTypeMatrixEncoding
	getPayloadSize (AVFrameDataMatrixEncoding x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataDownmixInfo where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeDownmixInfo)
	getPayloadType _ = AVFrameDataTypeDownmixInfo
	getPayloadSize (AVFrameDataDownmixInfo  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataReplayGain where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeReplayGain)
	getPayloadType _ = AVFrameDataTypeReplayGain
	getPayloadSize (AVFrameDataReplayGain  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataDisplayMatrix where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeDisplayMatrix)
	getPayloadType _ = AVFrameDataTypeDisplayMatrix
	getPayloadSize (AVFrameDataDisplayMatrix  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataAfd where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeAfd)
	getPayloadType _ = AVFrameDataTypeAfd
	getPayloadSize _ = 1
	peekPayload ptr _ = (AVFrameDataAfd).toCEnum.fromIntegral <$> (peek (castPtr ptr) :: IO Word8)
	pokePayload ptr (AVFrameDataAfd x) = poke (castPtr ptr) (fromIntegral.fromCEnum$ x :: Word8)
instance AVFrameSideDataPayload AVFrameDataMotionVectors where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeMotionVectors)
	getPayloadType _ = AVFrameDataTypeMotionVectors
	getPayloadSize (AVFrameDataMotionVectors  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke
instance AVFrameSideDataPayload AVFrameDataSkipSamples where
	peekAVFrameSideDataPtr ptr = castPtr <$>
		av_frame_get_side_data ptr (fromCEnum AVFrameDataTypeSkipSamples)
	getPayloadType _ = AVFrameDataTypeSkipSamples
	getPayloadSize (AVFrameDataSkipSamples  x) = fromIntegral$ sizeOf x
	peekPayload ptr _ = peek ptr
	pokePayload = poke

-- | Set side data for an AVFrame
frameSetSideData :: (MonadIO m, MonadError String m, AVFrameSideDataPayload a) =>
	AVFrame                -- ^ The frame to modify
	-> AVFrameSideData a   -- ^ The new side data.  If there is already side data
	                       --   of this type, then the old data will be replaced
	-> m ()
frameSetSideData frame sd = do
	let AVFrameSideData metadata payload = sd
	let sdType = getPayloadType payload
	let sdSize = getPayloadSize payload

	frameRemoveSideData frame sdType

	psd <- liftIO.withThis frame$ \ptr ->
		av_frame_new_side_data ptr (fromCEnum sdType) (fromIntegral sdSize)
	when (psd == nullPtr)$
		throwError$ "frameSetSideData: av_frame_new_side_data returned a null pointer"

	pdata <- liftIO (#{peek AVFrameSideData, data} psd :: IO (Ptr a))
	dsize <- liftIO (#{peek AVFrameSideData, size} psd :: IO (CInt))

	when (pdata == nullPtr)$
		throwError$ "frameSetSideData: av_frame_new_side_data did not allocate a buffer"
	when ((fromIntegral dsize) /= sdSize)$
		throwError$ "frameSetSideData: av_frame_new_side_data allocated a buffer of size " ++
			(show dsize) ++ " but we require " ++ (show sdSize) ++ " bytes"
	
	liftIO$ pokePayload pdata payload

	let pmetadata = psd `plusPtr` #{offset AVFrameSideData, metadata}
	withThis metadata$ \ppsrc -> liftIO$ do
		psrc <- peek ppsrc
		av_dict_copy pmetadata psrc 0

-- | Get the side data associated with an AVFrame
frameGetSideData :: forall a m. (MonadIO m, AVFrameSideDataPayload a) =>
	AVFrame -> m (Maybe (AVFrameSideData a))
frameGetSideData frame = liftIO.withThis frame$ \ptr -> do
	psd <- peekAVFrameSideDataPtr ptr :: IO (Ptr a)
	if psd == nullPtr then return Nothing else do
		pdata <- #{peek AVFrameSideData, data} psd :: IO (Ptr a)
		dsize <- #{peek AVFrameSideData, size} psd :: IO CInt
		metadata <- #{peek AVFrameSideData, metadata} psd :: IO (Ptr AVDictionary)
		if (pdata == nullPtr) then return Nothing else do
			m <- unsafeDictCopyFromPtr metadata []
			p <- peekPayload pdata (fromIntegral dsize)
			return.Just$ AVFrameSideData m p

-- | Explicitly free side data
frameRemoveSideData :: MonadIO m => AVFrame -> AVFrameSideDataType -> m ()
frameRemoveSideData frame sdtype = liftIO.withThis frame$ \ptr ->
	av_frame_remove_side_data ptr (fromCEnum sdtype)

-- | Get a String representation of an AVFrameSideDataType
frameSideDataName :: AVFrameSideDataType -> String
frameSideDataName sdType =
	-- safe because the c string is immutable
	unsafePerformIO.peekCString.av_frame_side_data_name$ fromCEnum sdType

