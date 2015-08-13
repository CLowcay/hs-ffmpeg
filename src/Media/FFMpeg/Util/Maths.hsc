{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
 
Description : Bindings to libavutil
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.Maths (
	AVTimestamp(..),
	av_nopts_value,
	av_time_base,
	av_time_base_q,
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

#include "ffmpeg.h"

import Data.Int
import Data.Ratio
import Foreign.C.Types
import Foreign.Storable

import Media.FFMpeg.Internal.Common

foreign import ccall "av_rescale_rnd" av_rescale_rnd :: Int64 -> Int64 -> Int64 -> CInt -> Int64

newtype AVTimestamp = AVTimestamp Int64 deriving (Eq, Ord, Num, Show, Storable)

av_nopts_value :: AVTimestamp
av_nopts_value = #{const AV_NOPTS_VALUE}

av_time_base :: Int64
av_time_base = #{const AV_TIME_BASE}

av_time_base_q :: Rational
av_time_base_q = 1 % (fromIntegral av_time_base)

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

