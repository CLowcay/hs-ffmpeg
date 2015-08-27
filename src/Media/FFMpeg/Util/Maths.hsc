{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE PatternSynonyms #-}

{- |
 
Description : Bindings to libavutil
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.Maths (
	AVTimestamp(..),
	pattern AVNoptsValue,
	pattern AVTimeBase,
	avTimeBaseQ,

	AVRound,
	pattern AVRoundZero,
	pattern AVRoundInf,
	pattern AVRoundDown,
	pattern AVRoundUp,
	pattern AVRoundNearInf,
	pattern AVRoundPassMinmax,

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

pattern AVNoptsValue = AVTimestamp (#{const AV_NOPTS_VALUE})
pattern AVTimeBase = (#{const AV_TIME_BASE})
avTimeBaseQ = 1 % (fromIntegral AVTimeBase)

newtype AVRound = AVRound CInt deriving (Show, Eq, CEnum)
pattern AVRoundZero = AVRound (#{const AV_ROUND_ZERO})
pattern AVRoundInf = AVRound (#{const AV_ROUND_INF})
pattern AVRoundDown = AVRound (#{const AV_ROUND_DOWN})
pattern AVRoundUp = AVRound (#{const AV_ROUND_UP})
pattern AVRoundNearInf = AVRound (#{const AV_ROUND_NEAR_INF})
pattern AVRoundPassMinmax = AVRound (#{const AV_ROUND_PASS_MINMAX})

rescaleRnd :: Int64 -> Int64 -> Int64 -> AVRound -> Int64
rescaleRnd a b c rnd = av_rescale_rnd a b c (fromCEnum rnd)

rescale :: Int64 -> Int64 -> Int64 -> Int64
rescale a b c = rescaleRnd a b c$ AVRoundNearInf

rescaleQRnd :: Int64 -> Rational -> Rational -> AVRound -> Int64
rescaleQRnd a bq cq rnd = let
	b = numerator bq * denominator cq
	c = numerator cq * denominator bq
	in rescaleRnd a (fromIntegral b) (fromIntegral c) rnd

-- | "rescale" an int by two rational numbers
rescaleQ :: Int64 -> Rational -> Rational -> Int64
rescaleQ a bq cq = rescaleQRnd a bq cq AVRoundNearInf

-- | "rescale" a timestamp by two rational numbers
rescaleTSQ :: AVTimestamp -> Rational -> Rational -> AVTimestamp
rescaleTSQ (AVTimestamp a) bq cq = AVTimestamp$ rescaleQ a bq cq

-- | "rescale" a timestamp with the given rounding move
rescaleTSQRnd :: AVTimestamp -> Rational -> Rational -> AVRound -> AVTimestamp
rescaleTSQRnd (AVTimestamp a) bq cq rnd = AVTimestamp$ rescaleQRnd a bq cq rnd

