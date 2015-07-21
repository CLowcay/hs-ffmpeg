{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.AVPicture (
	AVPicture (..),
	PlanarColor,
	makePlanarColor,
	pictureGetSize
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr

import Media.FFMpeg.Codec.AVFrame
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.Enums

foreign import ccall "avpicture_alloc" avpicture_alloc :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "avpicture_free" avpicture_free :: Ptr () -> IO ()
foreign import ccall "avpicture_fill" avpicture_fill :: Ptr () -> Ptr Word8 -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "avpicture_layout" avpicture_layout :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word8 -> CInt -> IO CInt
foreign import ccall "avpicture_get_size" avpicture_get_size :: CInt -> CInt -> CInt -> CInt
foreign import ccall "av_picture_copy" av_picture_copy :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO ()
foreign import ccall "av_picture_crop" av_picture_crop :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "av_picture_pad" av_picture_pad :: Ptr () -> Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

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
pictureGetSize :: MonadError String m => PixelFormat -> Int -> Int -> m Int
pictureGetSize pf width height = do
	let r = avpicture_get_size (fromCEnum pf) (fromIntegral width) (fromIntegral height)
	if r < 0 then
		throwError$ "pictureGetSize: failed with error code " ++ (show r)
	else return.fromIntegral$ r

-- | A class that represents the AVPicture struct
class ExternalPointer p => AVPicture p where
	-- | Allocate a buffer for a picture and execute a monadic action with that
	-- buffer.  The buffer is freed when the action returns, so the action must
	-- not return any references that depend on the buffer.
	pictureAlloc :: (MonadIO m, MonadError String m) =>
		p                -- ^ The picture to allocate the buffer for
		-> PixelFormat   -- ^ The pixel format for the buffer
		-> Int           -- ^ width
		-> Int           -- ^ height
		-> (p -> m b)    -- ^ the monadic action to execute
		-> m b
	-- | Connect an AVPicture to an external buffer.  The buffer is not managed
	-- by libav or hs-ffmeg.
	pictureFill :: (MonadIO m, MonadError String m) =>
		p                -- ^ The AVPicture
		-> Ptr b         -- ^ Pointer to a buffer
		-> PixelFormat   -- ^ The pixel format of the buffer
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> m ()
	-- | Copy an AVPicture into an external buffer.  The buffer is not managed by
	-- libav or hs-ffmpeg.
	pictureLayout :: (MonadIO m, MonadError String m) =>
		p                -- ^ The AVPicture
		-> PixelFormat   -- ^ The pixel format of the buffer
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> Ptr b         -- ^ Pointer to the buffer
		-> Int           -- ^ size of the buffer in bytes
		-> m Int         -- ^ the number of bytes written to the buffer
	-- | Copy image data.  Both the source and the destination must have
	-- sufficiently large buffers allocated.
	pictureCopy :: (MonadIO m, AVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> PixelFormat   -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ width in pixels
		-> Int           -- ^ height in pixels
		-> m ()
	-- | Crop an image top and left.  Both the source and the destination must
	-- have appropriate buffers allocated.
	pictureCrop :: (MonadIO m, AVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> PixelFormat   -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ top_band
		-> Int           -- ^ left_band
		-> m ()
	-- | Pad an image.  Both the source and the destination must have appropriate
	-- buffers allocated.
	picturePad :: (MonadIO m, AVPicture src) =>
		p                -- ^ destination
		-> src           -- ^ source
		-> Int           -- ^ source height
		-> Int           -- ^ source width
		-> PixelFormat   -- ^ The pixel format of the source and destination AVPictures
		-> Int           -- ^ top padding
		-> Int           -- ^ bottom padding
		-> Int           -- ^ left padding
		-> Int           -- ^ right padding
		-> PlanarColor   -- ^ the color to pad with
		-> m ()

instance AVPicture AVFrame where
	pictureAlloc picture format width height action = do
		r <- liftIO.withThis picture$ \ptr ->
			avpicture_alloc ptr (fromCEnum format) (fromIntegral width) (fromIntegral height)
		if r /= 0 then
			throwError$ "pictureAlloc: failed to allocate picture with error code " ++ (show r)
		else do
			r <- action picture
			liftIO.withThis picture$ \ptr -> avpicture_free ptr
			return r
	pictureFill picture pbuff format width height = do
		r <- liftIO.withThis picture$ \ptr ->
			avpicture_fill ptr (castPtr pbuff) (fromCEnum format) (fromIntegral width) (fromIntegral height)
		when (r < 0) $
			throwError$ "pictureFill: failed with error code " ++ (show r)
	pictureLayout picture format width height pbuff buffSize = do
		r <- liftIO.withThis picture$ \ptr ->
			avpicture_layout ptr (fromCEnum format)
			(fromIntegral width) (fromIntegral height)
			(castPtr pbuff) (fromIntegral buffSize)
		if r < 0 then
			throwError$ "pictureLayout: failed with error code " ++ (show r)
		else return.fromIntegral$ r
	pictureCopy dst src format width height = liftIO$
		withThis dst$ \pd ->
		withThis src$ \ps ->
			av_picture_copy pd ps (fromCEnum format)
				(fromIntegral width) (fromIntegral height)
	pictureCrop dst src format topBand leftBand = liftIO$
		withThis dst $ \pd ->
		withThis src $ \ps -> do
			av_picture_crop pd ps (fromCEnum format)
				(fromIntegral topBand) (fromIntegral leftBand)
			return ()
	picturePad dst src height width format top bottom left right (PlanarColor fpColor) = liftIO$
		withThis dst $ \pd ->
		withThis src $ \ps ->
		withForeignPtr fpColor$ \pc -> do
			av_picture_pad pd ps
				(fromIntegral height) (fromIntegral width) (fromCEnum format)
				(fromIntegral top) (fromIntegral bottom)
				(fromIntegral left) (fromIntegral right) pc
			return ()

