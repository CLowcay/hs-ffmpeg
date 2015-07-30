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
	module Media.FFMpeg.Util.Error,

	-- * Raw bindings
	av_free,
	av_freep,
	av_malloc,
	av_mallocz,
	av_realloc,
	pav_free,

	-- * AVTimestamp
	AVTimestamp(..),
	av_nopts_value,
	av_time_base,
	av_time_base_q,

	-- * Haskell interface to libavutil
	avMalloc,
	avMallocz,

	-- * Maths functions
	AVRound,
	av_round_zero,
	av_round_inf,
	av_round_down,
	av_round_up,
	av_round_near_inf,
	av_round_pass_minmax,
	rescaleRnd,
	rescale,
	rescaleQRnd,
	rescaleQ,
	rescaleTSQ
) where

import Control.Applicative
import Control.Monad.Except
import Data.Int
import Data.Ratio
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
import Media.FFMpeg.Util.Error

#include "ffmpeg.h"

foreign import ccall "av_free" av_free :: Ptr a -> IO ()
foreign import ccall "av_freep" av_freep :: Ptr (Ptr a) -> IO ()
-- | Pointer to av_free
foreign import ccall "&av_free" pav_free :: FunPtr (Ptr a -> IO ())
foreign import ccall "av_malloc" av_malloc :: CUInt -> IO (Ptr ())
foreign import ccall "av_mallocz" av_mallocz :: CUInt -> IO (Ptr ())
foreign import ccall "av_realloc" av_realloc :: Ptr () -> CSize -> IO (Ptr ())

foreign import ccall "av_rescale_rnd" av_rescale_rnd :: Int64 -> Int64 -> Int64 -> CInt -> Int64

newtype AVTimestamp = AVTimestamp Int64 deriving (Eq, Ord, Num, Show, Storable)

av_nopts_value :: AVTimestamp
av_nopts_value = #{const AV_NOPTS_VALUE}

av_time_base :: Int64
av_time_base = #{const AV_TIME_BASE}

av_time_base_q :: Rational
av_time_base_q = 1 % (fromIntegral av_time_base)

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

newtype AVRound = AVRound CInt deriving (Show, Eq, CEnum)
#{enum AVRound, AVRound,
	av_round_zero = AV_ROUND_ZERO,
	av_round_inf = AV_ROUND_INF,
	av_round_down = AV_ROUND_DOWN,
	av_round_up = AV_ROUND_UP,
	av_round_near_inf = AV_ROUND_NEAR_INF,
	av_round_pass_minmax = AV_ROUND_PASS_MINMAX
}

rescaleRnd :: Int64 -> Int64 -> Int64 -> AVRound -> Int64
rescaleRnd a b c rnd = av_rescale_rnd a b c (fromCEnum rnd)

rescale :: Int64 -> Int64 -> Int64 -> Int64
rescale a b c = rescaleRnd a b c$ av_round_near_inf

rescaleQRnd :: Int64 -> Rational -> Rational -> AVRound -> Int64
rescaleQRnd a bq cq rnd = let
	b = numerator bq * denominator cq
	c = numerator cq * denominator bq
	in rescaleRnd a (fromIntegral b) (fromIntegral c) rnd

-- | "rescale" an int by two rational numbers
rescaleQ :: Int64 -> Rational -> Rational -> Int64
rescaleQ a bq cq = rescaleQRnd a bq cq av_round_near_inf

-- | "rescale" a timestamp by two rational numbers
rescaleTSQ :: AVTimestamp -> Rational -> Rational -> AVTimestamp
rescaleTSQ (AVTimestamp a) bq cq = AVTimestamp$ rescaleQ a bq cq

