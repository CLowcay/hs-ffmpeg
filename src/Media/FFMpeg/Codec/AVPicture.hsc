{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.AVPicture (
	AVPicture(..),
	HasAVPicture(..),
	AVSubtitlePicture(..),
	makePlanarColor,
	pictureGetSize,
	PlanarColor
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Ratio
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "avpicture_alloc" avpicture_alloc :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "avpicture_free" avpicture_free :: Ptr () -> IO ()
foreign import ccall "avpicture_fill" avpicture_fill :: Ptr () -> Ptr Word8 -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "avpicture_layout" avpicture_layout :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "avpicture_get_size" avpicture_get_size :: CInt -> CInt -> CInt -> CInt
foreign import ccall "av_picture_copy" av_picture_copy :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "av_picture_crop" av_picture_crop :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "av_picture_pad" av_picture_pad :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

foreign import ccall "av_image_check_size" av_image_check_size :: CUInt -> CUInt -> CInt -> Ptr () -> IO CInt
foreign import ccall "b_av_image_check_sar" av_image_check_sar :: CUInt -> CUInt -> CInt -> CInt -> CInt

-- | A C array of color components.  The number of components, and their
-- meanings, depends on the pixel format being used.
newtype PlanarColor = PlanarColor (ForeignPtr CInt)

-- | Encode a list of color components in a PlanarColor
makePlanarColor :: MonadIO m => [Int] -> m PlanarColor
makePlanarColor cs = do
	fp <- liftIO.mallocForeignPtrArray$ (length cs)
	liftIO.withForeignPtr fp$ \ptr -> pokeArray ptr (fromIntegral <$> cs)
	return.PlanarColor$ fp

-- | Get the size of the buffer required to back an AVPicture
pictureGetSize :: MonadError String m => AVPixelFormat -> Int -> Int -> m Int
pictureGetSize pf width height = do
	let r = avpicture_get_size (fromCEnum pf) (fromIntegral width) (fromIntegral height)
	if r < 0 then
		throwError$ "pictureGetSize: failed with error code " ++ (show r)
	else return.fromIntegral$ r

-- | AVPicture struct
newtype AVPicture = AVPicture (ForeignPtr AVPicture)
instance ExternalPointer AVPicture where
	type UnderlyingType AVPicture = AVPicture
	withThis (AVPicture fp) = withThis fp

-- | A class that represents the AVPicture struct
class ExternalPointer p => HasAVPicture p where
	-- | Make a new AVPicture
	mkPicture :: (MonadIO m, MonadError String m) => m p

	-- | Get a pointer to the AVPicture struct
	withAVPicturePtr :: MonadIO m => p -> (Ptr (UnderlyingType p) -> m b) -> m b

	-- | Allocate a buffer for a picture and execute a monadic action with that
	-- buffer.  The buffer is freed when the action returns, so the action must
	-- not return any references that depend on the buffer.
	pictureAlloc :: (MonadIO m, MonadError String m) =>
		p                -- ^ The picture to allocate the buffer for
		-> AVPixelFormat -- ^ The pixel format for the buffer
		-> Int           -- ^ width
		-> Int           -- ^ height
		-> (p -> m b)    -- ^ the monadic action to execute
		-> m b
	pictureAlloc picture format width height action = do
		r <- withAVPicturePtr picture$ \ptr -> liftIO$ avpicture_alloc (castPtr ptr)
			(fromCEnum format) (fromIntegral width) (fromIntegral height)
		if r /= 0 then
			throwError$ "pictureAlloc: failed to allocate picture with error code " ++ (show r)
		else do
			r <- action picture
			withAVPicturePtr picture$ \ptr -> liftIO$ avpicture_free (castPtr ptr)
			return r

	-- | Connect an AVPicture to an external buffer.  The buffer is not managed
	-- by libav or hs-ffmeg.
	pictureFill :: (MonadIO m, MonadError String m) =>
		p                -- ^ The AVPicture
		-> Ptr b         -- ^ Pointer to a buffer
		-> AVPixelFormat -- ^ The pixel format of the buffer
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> m ()
	pictureFill picture pbuff format width height = do
		r <- withAVPicturePtr picture$ \ptr -> liftIO$
			avpicture_fill (castPtr ptr) (castPtr pbuff)
				(fromCEnum format) (fromIntegral width) (fromIntegral height)
		when (r < 0) $
			throwError$ "pictureFill: failed with error code " ++ (show r)

	-- | Copy an AVPicture into an external buffer.  The buffer is not managed by
	-- libav or hs-ffmpeg.
	pictureLayout :: (MonadIO m, MonadError String m) =>
		p                -- ^ The AVPicture
		-> AVPixelFormat -- ^ The pixel format of the buffer
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> Ptr b         -- ^ Pointer to the buffer
		-> Int           -- ^ size of the buffer in bytes
		-> m Int         -- ^ the number of bytes written to the buffer
	pictureLayout picture format width height pbuff buffSize = do
		r <- withAVPicturePtr picture$ \ptr ->
			liftIO$ avpicture_layout (castPtr ptr) (fromCEnum format)
				(fromIntegral width) (fromIntegral height)
				(castPtr pbuff) (fromIntegral buffSize)
		if r < 0 then
			throwError$ "pictureLayout: failed with error code " ++ (show r)
		else return.fromIntegral$ r

	-- | Copy image data.  Both the source and the destination must have
	-- sufficiently large buffers allocated.
	pictureCopy :: (MonadIO m, HasAVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> AVPixelFormat -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> m ()
	pictureCopy dst src format width height =
		withAVPicturePtr dst$ \pd ->
		withAVPicturePtr src$ \ps ->
			liftIO$ av_picture_copy (castPtr pd) (castPtr ps) (fromCEnum format)
				(fromIntegral width) (fromIntegral height)

	-- | Crop an image top and left.  Both the source and the destination must
	-- have appropriate buffers allocated.
	pictureCrop :: (MonadIO m, HasAVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> AVPixelFormat -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ top_band
		-> Int           -- ^ left_band
		-> m ()
	pictureCrop dst src format topBand leftBand =
		withAVPicturePtr dst $ \pd ->
		withAVPicturePtr src $ \ps -> do
			liftIO$ av_picture_crop (castPtr pd) (castPtr ps) (fromCEnum format)
				(fromIntegral topBand) (fromIntegral leftBand)
			return ()

	-- | Pad an image.  Both the source and the destination must have appropriate
	-- buffers allocated.
	picturePad :: (MonadIO m, HasAVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> Int           -- ^ source height
		-> Int           -- ^ source width
		-> AVPixelFormat -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ top padding
		-> Int           -- ^ bottom padding
		-> Int           -- ^ left padding
		-> Int           -- ^ right padding
		-> PlanarColor   -- ^ the color to pad with
		-> m ()
	picturePad dst src height width format top bottom left right (PlanarColor fpColor) =
		withAVPicturePtr dst $ \pd ->
		withAVPicturePtr src $ \ps ->
		liftIO.withForeignPtr fpColor$ \pc -> do
			av_picture_pad (castPtr pd) (castPtr ps)
				(fromIntegral height) (fromIntegral width) (fromCEnum format)
				(fromIntegral top) (fromIntegral bottom)
				(fromIntegral left) (fromIntegral right) pc
			return ()

instance HasAVPicture AVFrame where
	mkPicture = frameAlloc
	withAVPicturePtr = withThis

instance HasAVPicture AVPicture where
	mkPicture = do
		fp <- avMallocz #{size AVPicture}
		return.AVPicture$ fp
	withAVPicturePtr = withThis

-- | Special AVPicture type for subtitles
data AVSubtitlePicture = AVSubtitlePicture (ForeignPtr ()) Int
instance ExternalPointer AVSubtitlePicture where
	type UnderlyingType AVSubtitlePicture = ()
	withThis (AVSubtitlePicture fp _) = withThis fp

instance HasAVPicture AVSubtitlePicture where
	mkPicture = do
		fp <- avMallocz #{size AVPicture}
		return$ AVSubtitlePicture fp 0

	withAVPicturePtr (AVSubtitlePicture fp off) action = withThis fp (action.(`plusPtr` off))

-- | Determine if the image sample aspect ration is valid
imageCheckSAR :: Word -> Word -> AVRational -> Bool
imageCheckSAR w h (AVRational r) =
	(av_image_check_sar (fromIntegral w) (fromIntegral h)
		(fromIntegral$ numerator r)
		(fromIntegral$ denominator r)) == 0

