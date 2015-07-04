-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |Module 'Media.FFMpeg.SWScale' implements bindings to SWScale library

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.SWScale (
	libSWScaleVersion,
	module Media.FFMpeg.SWScaleEnums_,
	Context,
	scale,
	getContext
) where

#include "ffmpeg.h"

import Control.Applicative
import Data.Int
import Data.Monoid
import Data.Version
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Error
import Foreign.Ptr

import Media.FFMpeg.Common
import Media.FFMpeg.SWScaleEnums_
import Media.FFMpeg.Util

libSWScaleVersion :: Version
libSWScaleVersion = fromVersionNum #{const LIBSWSCALE_VERSION_INT}
-- 
-- |Context -- SwsContext implementation 
--
newtype Context = Context (ForeignPtr Context)

instance ExternalPointer Context where
    withThis (Context ctx) io = withForeignPtr ctx (io . castPtr)

--
-- |getContext - return SWScale context
--

foreign import ccall "sws_getContext" _sws_getContext :: 
    CInt -> CInt -> CInt -> 
    CInt -> CInt -> CInt ->
    CInt -> Ptr () -> Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall "sws_freeContext" _sws_freeContext :: Ptr () -> IO ()

getContext :: (Int, Int, PixelFormat) ->
              (Int, Int, PixelFormat) ->
              [ScaleAlgorithm] -> IO Context
getContext  (srcW, srcH, srcPf) (dstW, dstH, dstPf) flags = do
  ret <- throwIf (==nullPtr)
         (\_ -> "getContext: failed to create SWScale context")
         (_sws_getContext (cFromInt srcW) (cFromInt srcH) (fromCEnum srcPf)
           (cFromInt dstW) (cFromInt dstH) (fromCEnum dstPf)
           (fromCEnum.mconcat$ flags) nullPtr nullPtr nullPtr)
  (Context . castForeignPtr) <$> newFinForeignPtr _sws_freeContext ret
           

--
-- | scale - a scale function
--

foreign import ccall "sws_scale" _sws_scale ::
    Ptr () -> Ptr () -> Ptr () -> CInt -> CInt -> Ptr () -> Ptr () -> IO CInt

scale :: Context -> Ptr () -> Ptr () -> Int -> Int -> Ptr () -> Ptr () -> IO ()
scale ctx srcSlice srcStride srcSliceY srcSliceH dstSlice dstStride =
    withThis ctx $ \ctx' ->
        _sws_scale ctx' srcSlice srcStride 
             (cFromInt srcSliceY) (cFromInt srcSliceH)
             dstSlice dstStride >> return ()

