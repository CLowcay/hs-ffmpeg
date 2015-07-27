{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |

Description : Bindings to libavutil
Copyright   : (c) Vasyl Pasternak, 2009
            :     Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util (
	module Media.FFMpeg.Util.AVFrame,
	module Media.FFMpeg.Util.AVFrameSideData,
	module Media.FFMpeg.Util.Buffer,
	module Media.FFMpeg.Util.ChannelLayout,
	module Media.FFMpeg.Util.Dict,
	module Media.FFMpeg.Util.Enums,

	-- * Raw bindings
	av_free,
	av_freep,
	av_malloc,
	av_mallocz,
	av_realloc,
	pav_free,

	-- * AVTimestamp
	AVTimestamp(..),

	-- * Haskell interface to libavutil
	avMalloc,
	avMallocz
) where

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Error
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrame
import Media.FFMpeg.Util.AVFrameSideData
import Media.FFMpeg.Util.Buffer
import Media.FFMpeg.Util.ChannelLayout
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums

#include "ffmpeg.h"

foreign import ccall "av_free" av_free :: Ptr a -> IO ()
foreign import ccall "av_freep" av_freep :: Ptr (Ptr a) -> IO ()
-- | Pointer to av_free
foreign import ccall "&av_free" pav_free :: FunPtr (Ptr a -> IO ())
foreign import ccall "av_malloc" av_malloc :: CUInt -> IO (Ptr ())
foreign import ccall "av_mallocz" av_mallocz :: CUInt -> IO (Ptr ())
foreign import ccall "av_realloc" av_realloc :: Ptr () -> CSize -> IO (Ptr ())

newtype AVTimestamp = AVTimestamp Int64 deriving (Eq, Ord, Num, Show, Storable)

-- | Safely allocate a ForeignPtr with av_malloc
avMalloc :: (MonadIO m, MonadError String m) => Word -> m (ForeignPtr b)
avMalloc size = do
	ptr <- liftIO$ av_malloc (fromIntegral size)
	if (ptr == nullPtr)
		then throwError "avMalloc: allocated a null pointer"
		else liftIO$ newForeignPtr pav_free (castPtr ptr)

-- | Safely allocate a ForeignPtr with av_mallocz
avMallocz :: (MonadIO m, MonadError String m) => Word -> m (ForeignPtr b)
avMallocz size = do
	ptr <- liftIO$ av_mallocz (fromIntegral size)
	if (ptr == nullPtr)
		then throwError "avMallocz: allocated a null pointer"
		else liftIO$ newForeignPtr pav_free (castPtr ptr)

