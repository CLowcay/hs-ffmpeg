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
	module Media.FFMpeg.Util.ChannelLayout,
	module Media.FFMpeg.Util.Dict,
	module Media.FFMpeg.Util.Enums,

	-- * Raw bindings
	av_free,
	av_freep,
	pav_free,
	av_malloc,

	-- * AVTimestamp
	AVTimestamp(..),

	-- * Haskell interface to libavutil
	avMalloc,

	-- * Buffer management
	Buffer,
	allocBuffer,
	bufferSize
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
import Text.Printf

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrame
import Media.FFMpeg.Util.AVFrameSideData
import Media.FFMpeg.Util.ChannelLayout
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums

#include "ffmpeg.h"

foreign import ccall "av_free" av_free :: Ptr a -> IO ()
foreign import ccall "av_freep" av_freep :: Ptr a -> IO ()
-- | Pointer to av_free
foreign import ccall "&av_free" pav_free :: FunPtr (Ptr a -> IO ())
foreign import ccall "av_malloc" av_malloc :: CUInt -> IO (Ptr ())

newtype AVTimestamp = AVTimestamp Int64 deriving (Eq, Ord, Num, Show)

-- | Safely allocate a ForeignPtr with av_malloc
avMalloc :: (MonadIO m, MonadError String m) => Word -> m (ForeignPtr ())
avMalloc size = do
	ptr <- liftIO$ av_malloc (fromIntegral size)
	if (ptr == nullPtr)
		then throwError "avMalloc: allocated a null pointer"
		else liftIO$ newForeignPtr pav_free ptr

-- | A convenient Buffer object
data Buffer = Buffer !Int !(ForeignPtr ()) !(Ptr ())

instance ExternalPointer Buffer where
	withThis (Buffer _ fp ptr) io = do
		r <- io$ castPtr ptr
		touchForeignPtr fp
		return r
	
-- | Allocate a buffer
allocBuffer :: MonadIO m =>
	Int                -- ^ the size of the buffer
	-> m Buffer
allocBuffer size = do
	fp <- liftIO$ mallocForeignPtrBytes size
	return$ Buffer size fp (unsafeForeignPtrToPtr fp)
		-- safe because the Ptr is always paired with a reference to the ForeignPtr

-- | The size of a Buffer in bytes
bufferSize :: Buffer -> Int
bufferSize (Buffer size _ _) = size

