{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
	module Media.FFMpeg.Util.Error,
	module Media.FFMpeg.Util.Maths,
	module Media.FFMpeg.Util.Options,

	-- * Raw bindings
	av_free,
	av_freep,
	av_malloc,
	av_mallocz,
	av_realloc,
	pav_free,

	-- * Indexes and ids
	StreamIndex(..),
	ProgramID(..),

	-- * Haskell interface to libavutil
	avMalloc,
	avMallocz,
	ts2str,
	ts2timestr
) where

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Ratio
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Text.Printf

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.AVFrame
import Media.FFMpeg.Util.AVFrameSideData
import Media.FFMpeg.Util.Buffer
import Media.FFMpeg.Util.ChannelLayout
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums
import Media.FFMpeg.Util.Error
import Media.FFMpeg.Util.Maths
import Media.FFMpeg.Util.Options hiding (AVClass(..))
import Media.FFMpeg.Util.Options (AVClass)

#include "ffmpeg.h"

foreign import ccall "av_free" av_free :: Ptr a -> IO ()
foreign import ccall "av_freep" av_freep :: Ptr (Ptr a) -> IO ()
-- | Pointer to av_free
foreign import ccall "&av_free" pav_free :: FunPtr (Ptr a -> IO ())
foreign import ccall "av_malloc" av_malloc :: CUInt -> IO (Ptr ())
foreign import ccall "av_mallocz" av_mallocz :: CUInt -> IO (Ptr ())
foreign import ccall "av_realloc" av_realloc :: Ptr () -> CSize -> IO (Ptr ())

-- | Type for stream indexes
newtype StreamIndex = StreamIndex CInt deriving (Eq, Ord, Show, Enum, Storable)

-- | Type for program ids
newtype ProgramID = ProgramID CInt deriving (Eq, Ord, Show)

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

foreign import ccall "memset" memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)
foreign import ccall "b_av_ts_make_string" b_av_ts_make_string :: CString -> Int64 -> IO CString
foreign import ccall "b_av_ts_make_time_string" b_av_ts_make_time_string :: CString -> Int64 -> Ptr (Maybe AVRational) -> IO CString

-- | Convert a timestamp to a string
ts2str :: AVTimestamp -> String
ts2str (AVTimestamp ts) = unsafePerformIO$
	allocaArray #{const AV_TS_MAX_STRING_SIZE}$ \ps -> do
		memset ps 0 #{const AV_TS_MAX_STRING_SIZE}
		b_av_ts_make_string ps ts
		peekCString ps

-- | Convert a timestamp to a string with a specified time_base
ts2timestr :: AVTimestamp -> AVRational -> String
ts2timestr (AVTimestamp ts) tb = unsafePerformIO$
	allocaArray #{const AV_TS_MAX_STRING_SIZE}$ \ps -> do
	alloca$ \pr -> do
		poke pr (Just tb)
		memset ps 0 #{const AV_TS_MAX_STRING_SIZE}
		b_av_ts_make_time_string ps ts pr
		peekCString ps

