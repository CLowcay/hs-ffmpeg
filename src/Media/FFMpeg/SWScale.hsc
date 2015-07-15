-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts #-}

{- |

Description : Bindings to libswscale
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libswscale.

-}

module Media.FFMpeg.SWScale (
	module Media.FFMpeg.SWScale.Enums,
	-- * raw bindings
	sws_getContext,
	psws_freeContext,
	sws_scale,

	-- * Haskell interface to libswscale
	libSWScaleVersion,
	SwsContext,
	getContext,
	scale
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Monoid
import Data.Version
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Error
import Foreign.Ptr

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Enums
import Media.FFMpeg.Util

foreign import ccall "sws_getContext" sws_getContext :: 
	CInt -> CInt -> CInt -> 
	CInt -> CInt -> CInt ->
	CInt -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())

-- | Pointer to sws_freeContext
foreign import ccall "&sws_freeContext" psws_freeContext ::
	FunPtr (Ptr () -> IO ())

foreign import ccall "sws_scale" sws_scale ::
	Ptr () -> Ptr () -> Ptr () -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt

-- | Which version of libswscale are we using?
libSWScaleVersion :: Version
libSWScaleVersion = fromVersionNum #{const LIBSWSCALE_VERSION_INT}

-- | SwsContext struct
newtype SwsContext = SwsContext (ForeignPtr SwsContext)

instance ExternalPointer SwsContext where
    withThis (SwsContext ctx) io = withForeignPtr ctx (io . castPtr)

-- | Get SWScale context
getContext :: (MonadIO m, MonadError String m) =>
	(Int, Int, PixelFormat)       -- ^ source (width, height, pixel format)
	-> (Int, Int, PixelFormat)    -- ^ destination (width, height, pixel format)
	-> [ScaleAlgorithm]           -- ^ the algorithm to use and its options
	-> m SwsContext
getContext  (srcW, srcH, srcPf) (dstW, dstH, dstPf) flags = do
	ret <- liftIO$ sws_getContext
		(fromIntegral srcW) (fromIntegral srcH) (fromCEnum srcPf)
		(fromIntegral dstW) (fromIntegral dstH) (fromCEnum dstPf)
		(fromCEnum.mconcat$ flags) nullPtr nullPtr nullPtr
	if (ret == nullPtr)
		then throwError "getContext: sws_getContext returned a null pointer"
		else liftIO$ (SwsContext . castForeignPtr) <$> newForeignPtr psws_freeContext ret

-- | Scale the source image into the destination buffer
scale ::
	SwsContext   -- ^ the scaling context
	-> Ptr ()    -- ^ the source slice (array of planes)
	-> Ptr ()    -- ^ the source stride (array of strides for each plane)
	-> Int       -- ^ source Y
	-> Int       -- ^ source H
	-> Ptr ()    -- ^ the destination slice (array of planes)
	-> Ptr ()    -- ^ the destination stride (array of strides for each plane)
	-> IO ()
scale ctx srcSlice srcStride srcSliceY srcSliceH dstSlice dstStride =
	withThis ctx $ \ctx' -> do
		sws_scale ctx' srcSlice srcStride 
			(fromIntegral srcSliceY) (fromIntegral srcSliceH)
			dstSlice dstStride
		return ()

