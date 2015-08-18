{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |

Description : AVPacketSideData and related functions
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.AVPacketSideData (
	AVPacketSideDataPalette(..),
	AVPacketSideDataNewExtradata(..),
	AVPacketSideDataParamChange(..),
	AVPacketSideDataH263MbInfo(..),
	AVPacketSideDataReplayGain(..),
	AVPacketSideDataDisplayMatrix(..),
	AVPacketSideDataStereo3d(..),
	AVPacketSideDataAudioServiceType(..),
	AVPacketSideDataQualityFactor(..),
	AVPacketSideDataSkipSamples(..),
	AVPacketSideDataJpDualmono(..),
	AVPacketSideDataStringsMetadata(..),
	AVPacketSideDataSubtitlePosition(..),
	AVPacketSideDataMatroskaBlockadditional(..),
	AVPacketSideDataWebvttIdentifier(..),
	AVPacketSideDataWebvttSettings(..),
	AVPacketSideDataMetadataUpdate(..),

	AVPacketSideData(..),

	AVPacketParamChange(..),
	H263MBInfo(..),
	DataJPDualMono(..),
	jp_dual_mono_mail,
	jp_dual_mono_left,
	jp_dual_mono_sub,
	jp_dual_mono_right,
	jp_dual_mono_both,
	SubtitlePosition(..)
) where

#include "ffmpeg.h"

import Control.Applicative
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B

import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Util

newtype AVPacketSideDataPalette = AVPacketSideDataPalette B.ByteString
newtype AVPacketSideDataNewExtradata = AVPacketSideDataNewExtradata B.ByteString
newtype AVPacketSideDataParamChange = AVPacketSideDataParamChange AVPacketParamChange deriving Storable
newtype AVPacketSideDataH263MbInfo = AVPacketSideDataH263MbInfo H263MBInfo deriving Storable
newtype AVPacketSideDataReplayGain = AVPacketSideDataReplayGain AVReplayGain deriving Storable
newtype AVPacketSideDataDisplayMatrix = AVPacketSideDataDisplayMatrix DisplayMatrix deriving Storable
newtype AVPacketSideDataStereo3d = AVPacketSideDataStereo3d AVStereo3D deriving Storable
newtype AVPacketSideDataAudioServiceType = AVPacketSideDataAudioServiceType AVAudioServiceType deriving Storable
newtype AVPacketSideDataQualityFactor = AVPacketSideDataQualityFactor CInt deriving Storable
newtype AVPacketSideDataSkipSamples = AVPacketSideDataSkipSamples SkipSamples deriving Storable
newtype AVPacketSideDataJpDualmono = AVPacketSideDataJpDualmono DataJPDualMono deriving Storable
newtype AVPacketSideDataStringsMetadata = AVPacketSideDataStringsMetadata AVDictionary
newtype AVPacketSideDataSubtitlePosition = AVPacketSideDataSubtitlePosition SubtitlePosition deriving Storable
newtype AVPacketSideDataMatroskaBlockadditional = AVPacketSideDataMatroskaBlockadditional B.ByteString
newtype AVPacketSideDataWebvttIdentifier = AVPacketSideDataWebvttIdentifier String
newtype AVPacketSideDataWebvttSettings = AVPacketSideDataWebvttSettings String
newtype AVPacketSideDataMetadataUpdate = AVPacketSideDataMetadataUpdate AVDictionary

data AVPacketSideData a = AVPacketSideData a

data AVPacketParamChange =
	AVPacketParamChangeChannelCount Int32 |
	AVPacketParamChangeChannelLayout Word64 |
	AVPacketParamChangeChannelSampleRate Int32 |
	AVPacketParamChangeChannelDimensions {
		change_channel_dimensions_width :: Int32,
		change_channel_dimensions_height :: Int32
	}

instance Storable AVPacketParamChange where
	sizeOf (AVPacketParamChangeChannelCount _) = 4
	sizeOf (AVPacketParamChangeChannelLayout _) = 8
	sizeOf (AVPacketParamChangeChannelSampleRate _) = 4
	sizeOf (AVPacketParamChangeChannelDimensions _ _) = 8
	alignment _ = 8
	peek ptr = do
		flags <- peek (castPtr ptr) :: IO AVSideDataParamChangeFlags
		let ptrData = ptr `plusPtr` 4
		case flags of
			AVSideDataParamChangeChannelCount ->
				AVPacketParamChangeChannelCount <$> peek ptrData
			AVSideDataParamChangeChannelLayout ->
				AVPacketParamChangeChannelLayout <$> peek ptrData
			AVSideDataParamChangeSampleRate ->
				AVPacketParamChangeChannelSampleRate <$> peek ptrData
			AVSideDataParamChangeDimensions ->
				AVPacketParamChangeChannelDimensions <$>
					(peek ptrData) <*> (peek$ ptrData `plusPtr` 4)
			_ -> fail$
				"Attempted to marshal an AVPacketParamChange into Haskell, but the flag field was invalid."
				++ "  The flag field was " ++ (show flags)
	poke ptr (AVPacketParamChangeChannelCount v) = do
		poke (castPtr ptr) AVSideDataParamChangeChannelCount
		poke (ptr `plusPtr` 4) v
	poke ptr (AVPacketParamChangeChannelLayout v) = do
		poke (castPtr ptr) AVSideDataParamChangeChannelLayout
		poke (ptr `plusPtr` 4) v
	poke ptr (AVPacketParamChangeChannelSampleRate v) = do
		poke (castPtr ptr) AVSideDataParamChangeSampleRate
		poke (ptr `plusPtr` 4) v
	poke ptr (AVPacketParamChangeChannelDimensions w h) = do
		poke (castPtr ptr) AVSideDataParamChangeDimensions
		poke (ptr `plusPtr` 4) w
		poke (ptr `plusPtr` 8) h

data H263MBInfo = H263MBInfo {
		h263mbinfo_offset :: Word32,
		h263mbinfo_quantizer :: Word8,
		h263mbinfo_gob :: Word8,
		h263mbinfo_mb_address :: Word16,
		h263mbinfo_h_mv_predictor :: Word8,
		h263mbinfo_v_mv_predictor :: Word8,
		h263mbinfo_h_mv_predictor3 :: Word8,
		h263mbinfo_v_mv_predictor3 :: Word8
	}

instance Storable H263MBInfo where
	sizeOf _ = 4 + 2 + 2 + 4
	alignment _ = 8
	peek ptr = do
		_offset <-          peek (castPtr ptr)
		_quantizer <-       peek (ptr `plusPtr` 4)
		_gob <-             peek (ptr `plusPtr` 5)
		_mb_address <-      peek (ptr `plusPtr` 6)
		_h_mv_predictor <-  peek (ptr `plusPtr` 8)
		_v_mv_predictor <-  peek (ptr `plusPtr` 9)
		_h_mv_predictor3 <- peek (ptr `plusPtr` 10)
		_v_mv_predictor3 <- peek (ptr `plusPtr` 11)
		return$ H263MBInfo
			_offset
			_quantizer
			_gob
			_mb_address
			_h_mv_predictor
			_v_mv_predictor
			_h_mv_predictor3
			_v_mv_predictor3
	poke ptr hi = do
		poke (castPtr ptr)      (h263mbinfo_offset hi)
		poke (ptr `plusPtr` 4)  (h263mbinfo_quantizer hi)
		poke (ptr `plusPtr` 5)  (h263mbinfo_gob hi)
		poke (ptr `plusPtr` 6)  (h263mbinfo_mb_address hi)
		poke (ptr `plusPtr` 8)  (h263mbinfo_h_mv_predictor hi)
		poke (ptr `plusPtr` 9)  (h263mbinfo_v_mv_predictor hi)
		poke (ptr `plusPtr` 10) (h263mbinfo_h_mv_predictor3 hi)
		poke (ptr `plusPtr` 11) (h263mbinfo_v_mv_predictor3 hi)

newtype DataJPDualMono = DataJPDualMono Word8 deriving (Eq, Storable)
jp_dual_mono_mail = DataJPDualMono 0
jp_dual_mono_left = DataJPDualMono 0
jp_dual_mono_sub = DataJPDualMono 1
jp_dual_mono_right = DataJPDualMono 1
jp_dual_mono_both = DataJPDualMono 2

data SubtitlePosition = SubtitlePosition {
	subtitle_position_x1 :: Word32,
	subtitle_position_y1 :: Word32,
	subtitle_position_x2 :: Word32,
	subtitle_position_y2 :: Word32
}

instance Storable SubtitlePosition where
	sizeOf _ = 4 * 4
	alignment _ = 8
	peek ptr = do
		_x1 <- peek (castPtr ptr)
		_y1 <- peek (ptr `plusPtr` 4)
		_x2 <- peek (ptr `plusPtr` 8)
		_y2 <- peek (ptr `plusPtr` 12)
		return$ SubtitlePosition
			_x1 _y1
			_x2 _y2
	poke ptr sp = do
		poke (castPtr ptr)      (subtitle_position_x1 sp)
		poke (ptr `plusPtr` 4)  (subtitle_position_y1 sp)
		poke (ptr `plusPtr` 8)  (subtitle_position_x2 sp)
		poke (ptr `plusPtr` 12) (subtitle_position_y2 sp)

