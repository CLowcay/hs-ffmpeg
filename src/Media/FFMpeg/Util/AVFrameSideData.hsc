{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Description : Bindings to libavutil.
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.AVFrameSideData (
	AVFrameDataPanScan(..),
	AVFrameDataA53CC(..),
	AVFrameDataStereo3d(..),
	AVFrameDataMatrixEncoding(..),
	AVFrameDataDownmixInfo(..),
	AVFrameDataReplayGain(..),
	AVFrameDataDisplayMatrix(..),
	AVFrameDataAfd(..),
	AVFrameDataMotionVectors(..),
	AVFrameDataSkipSamples(..),

	AVFrameSideData(..),
	AVPanScan(..),
	AVStereo3D(..),
	AVDownmixInfo(..),
	AVReplayGain(..),
	DisplayMatrix(..),
	AVMotionVector(..),
	SkipSamplesReason(..),
	SkipSamples(..)
) where

#include "ffmpeg.h"

import Control.Applicative
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.ChannelLayout
import Media.FFMpeg.Util.Dict
import Media.FFMpeg.Util.Enums

newtype AVFrameDataPanScan = AVFrameDataPanScan AVPanScan deriving Storable
newtype AVFrameDataA53CC = AVFrameDataA53CC B.ByteString
newtype AVFrameDataStereo3d = AVFrameDataStereo3d AVStereo3D deriving Storable
newtype AVFrameDataMatrixEncoding = AVFrameDataMatrixEncoding AVMatrixEncoding deriving Storable
newtype AVFrameDataDownmixInfo = AVFrameDataDownmixInfo AVDownmixInfo deriving Storable
newtype AVFrameDataReplayGain = AVFrameDataReplayGain AVReplayGain deriving Storable
newtype AVFrameDataDisplayMatrix = AVFrameDataDisplayMatrix DisplayMatrix deriving Storable
newtype AVFrameDataAfd = AVFrameDataAfd AVActiveFormatDescription
newtype AVFrameDataMotionVectors = AVFrameDataMotionVectors AVMotionVector deriving Storable
newtype AVFrameDataSkipSamples = AVFrameDataSkipSamples SkipSamplesReason deriving Storable

-- | Side data which can be associated with an AVFrame
data AVFrameSideData a = AVFrameSideData {
		frameSideData_metadata :: AVDictionary,
		frameSideData_payload :: a
	}

-- | AVPanScan struct
data AVPanScan = AVPanScan {
		avPanScan_id :: CInt,
		avPanScan_width :: CInt,
		avPanScan_height :: CInt,
		avPanScan_pos0 :: (Int16, Int16),
		avPanScan_pos1 :: (Int16, Int16),
		avPanScan_pos2 :: (Int16, Int16)
	}

instance Storable AVPanScan where
	sizeOf _ = #{size AVPanScan}
	alignment _ = 8
	peek ptr = do
		_id <- #{peek AVPanScan, id} ptr
		_width <- #{peek AVPanScan, width} ptr
		_height <- #{peek AVPanScan, height} ptr
		let pPosition = ptr `plusPtr` #{offset AVPanScan, position}
		_pos00 <- peekElemOff pPosition 0
		_pos01 <- peekElemOff pPosition 1
		_pos10 <- peekElemOff pPosition 2
		_pos11 <- peekElemOff pPosition 3
		_pos20 <- peekElemOff pPosition 4
		_pos21 <- peekElemOff pPosition 5
		return$ AVPanScan _id _width _height
			(_pos00, _pos01) (_pos10, _pos11) (_pos20, _pos21)
	poke ptr avps = do
		#{poke AVPanScan, id} ptr (avPanScan_id avps)
		#{poke AVPanScan, width} ptr (avPanScan_width avps)
		#{poke AVPanScan, height} ptr (avPanScan_height avps)
		let pPosition = ptr `plusPtr` #{offset AVPanScan, position}
		pokeElemOff pPosition 0 (fst.avPanScan_pos0$ avps)
		pokeElemOff pPosition 1 (snd.avPanScan_pos0$ avps)
		pokeElemOff pPosition 2 (fst.avPanScan_pos1$ avps)
		pokeElemOff pPosition 3 (snd.avPanScan_pos1$ avps)
		pokeElemOff pPosition 4 (fst.avPanScan_pos2$ avps)
		pokeElemOff pPosition 5 (snd.avPanScan_pos2$ avps)

-- | AVStereo3D struct
data AVStereo3D = AVStereo3D {
		avStereo3D_type :: AVStereo3DType,
		avStereo3D_flags :: AVStereo3DFlags
	}

instance Storable AVStereo3D where
	sizeOf _ = #{size AVStereo3D}
	alignment _ = 8
	peek ptr = do
		_type <- toCEnum <$> #{peek AVStereo3D, type} ptr
		_flags <- toCEnum <$> #{peek AVStereo3D, flags} ptr
		return$ AVStereo3D _type _flags
	poke ptr avs3d = do
		#{poke AVStereo3D, type} ptr (fromCEnum.avStereo3D_type$ avs3d)
		#{poke AVStereo3D, flags} ptr (fromCEnum.avStereo3D_flags$ avs3d)

-- | AVDownmixInfo struct
data AVDownmixInfo = AVDownmixInfo {
		avDownmixInfo_preferred_downmix_type :: AVDownmixType,
		avDownmixInfo_center_mix_level :: Double,
		avDownmixInfo_center_mix_level_ltrt :: Double,
		avDownmixInfo_surround_mix_level :: Double,
		avDownmixInfo_surround_mix_level_ltrt :: Double,
		avDownmixInfo_lfe_mix_level :: Double
	}

instance Storable AVDownmixInfo where
	sizeOf _ = #{size AVDownmixInfo}
	alignment _ = 8
	peek ptr = do
		_preferred_downmix_type <- toCEnum <$> #{peek AVDownmixInfo, preferred_downmix_type} ptr
		_center_mix_level <- #{peek AVDownmixInfo, center_mix_level} ptr
		_center_mix_level_ltrt <- #{peek AVDownmixInfo, center_mix_level_ltrt} ptr
		_surround_mix_level <- #{peek AVDownmixInfo, surround_mix_level} ptr
		_surround_mix_level_ltrt <- #{peek AVDownmixInfo, surround_mix_level_ltrt} ptr
		_lfe_mix_level <- #{peek AVDownmixInfo, lfe_mix_level} ptr
		return$ AVDownmixInfo
			_preferred_downmix_type
			_center_mix_level
			_center_mix_level_ltrt
			_surround_mix_level
			_surround_mix_level_ltrt
			_lfe_mix_level
	poke ptr avdi = do
		#{poke AVDownmixInfo, preferred_downmix_type} ptr (fromCEnum.avDownmixInfo_preferred_downmix_type$ avdi)
		#{poke AVDownmixInfo, center_mix_level} ptr (avDownmixInfo_center_mix_level avdi)
		#{poke AVDownmixInfo, center_mix_level_ltrt} ptr (avDownmixInfo_center_mix_level_ltrt avdi)
		#{poke AVDownmixInfo, surround_mix_level} ptr (avDownmixInfo_surround_mix_level avdi)
		#{poke AVDownmixInfo, surround_mix_level_ltrt} ptr (avDownmixInfo_surround_mix_level_ltrt avdi)
		#{poke AVDownmixInfo, lfe_mix_level} ptr (avDownmixInfo_lfe_mix_level avdi)

-- | AVReplayGain struct
data AVReplayGain = AVReplayGain {
		avReplayGain_track_gain :: Int32,
		avReplayGain_track_peak :: Word32,
		avReplayGain_album_gain :: Int32,
		avReplayGain_album_peak :: Word32
	}

instance Storable AVReplayGain where
	sizeOf _ = #{size AVReplayGain}
	alignment _ = 8
	peek ptr = do
		_track_gain <- #{peek AVReplayGain, track_gain} ptr
		_track_peak <- #{peek AVReplayGain, track_peak} ptr
		_album_gain <- #{peek AVReplayGain, album_gain} ptr
		_album_peak <- #{peek AVReplayGain, album_peak} ptr
		return$ AVReplayGain
			_track_gain _track_peak
			_album_gain _album_peak
	poke ptr avrg = do
		#{poke AVReplayGain, track_gain} ptr (avReplayGain_track_gain avrg)
		#{poke AVReplayGain, track_peak} ptr (avReplayGain_track_peak avrg)
		#{poke AVReplayGain, album_gain} ptr (avReplayGain_album_gain avrg)
		#{poke AVReplayGain, album_peak} ptr (avReplayGain_album_peak avrg)

-- | A representation of display matrix data
data DisplayMatrix = DisplayMatrix
	Double Double Double
	Double Double Double
	Double Double Double

instance Storable DisplayMatrix where
	sizeOf _ = 32 * 9
	alignment _ = 8
	peek ptr = do
		let pa = castPtr ptr :: Ptr Int32
		a <- (/ (2^16)).fromIntegral <$> peekElemOff pa 0
		b <- (/ (2^16)).fromIntegral <$> peekElemOff pa 1
		u <- (/ (2^30)).fromIntegral <$> peekElemOff pa 2
		c <- (/ (2^16)).fromIntegral <$> peekElemOff pa 3
		d <- (/ (2^16)).fromIntegral <$> peekElemOff pa 4
		v <- (/ (2^30)).fromIntegral <$> peekElemOff pa 5
		e <- (/ (2^16)).fromIntegral <$> peekElemOff pa 6
		f <- (/ (2^16)).fromIntegral <$> peekElemOff pa 7
		w <- (/ (2^30)).fromIntegral <$> peekElemOff pa 8
		return$ DisplayMatrix a b u c d v e f w
	poke ptr (DisplayMatrix a b u c d v e f w) = do
		let pa = castPtr ptr :: Ptr Int32
		pokeElemOff pa 0.round$ a * (2^16)
		pokeElemOff pa 1.round$ b * (2^16)
		pokeElemOff pa 2.round$ u * (2^30)
		pokeElemOff pa 3.round$ c * (2^16)
		pokeElemOff pa 4.round$ d * (2^16)
		pokeElemOff pa 5.round$ v * (2^30)
		pokeElemOff pa 6.round$ e * (2^16)
		pokeElemOff pa 7.round$ f * (2^16)
		pokeElemOff pa 8.round$ w * (2^30)

-- | AVMotionVector struct
data AVMotionVector = AVMotionVector {
		avMotionVector_source :: Int32,
		avMotionVector_w :: Word8,
		avMotionVector_h :: Word8,
		avMotionVector_src_x :: Int16,
		avMotionVector_src_y :: Int16,
		avMotionVector_dst_x :: Int16,
		avMotionVector_dst_y :: Int16,
		avMotionVector_flags :: Word64
	}

instance Storable AVMotionVector where
	sizeOf _ = #{size AVMotionVector}
	alignment _ = 8
	peek ptr = do
		_source <- #{peek AVMotionVector, source } ptr
		_w <- #{peek AVMotionVector, w} ptr
		_h <- #{peek AVMotionVector, h} ptr
		_src_x <- #{peek AVMotionVector, src_x} ptr
		_src_y <- #{peek AVMotionVector, src_y} ptr
		_dst_x <- #{peek AVMotionVector, dst_x} ptr
		_dst_y <- #{peek AVMotionVector, dst_y} ptr
		_flags <- #{peek AVMotionVector, flags} ptr
		return$ AVMotionVector
			_source _w _h
			_src_x _src_y
			_dst_x _dst_y _flags
	poke ptr avmv = do
		#{poke AVMotionVector, source} ptr (avMotionVector_source avmv)
		#{poke AVMotionVector, w} ptr (avMotionVector_w avmv)
		#{poke AVMotionVector, h} ptr (avMotionVector_h avmv)
		#{poke AVMotionVector, src_x} ptr (avMotionVector_src_x avmv)
		#{poke AVMotionVector, src_y} ptr (avMotionVector_src_y avmv)
		#{poke AVMotionVector, dst_x} ptr (avMotionVector_dst_x avmv)
		#{poke AVMotionVector, dst_y} ptr (avMotionVector_dst_y avmv)
		#{poke AVMotionVector, flags} ptr (avMotionVector_flags avmv)

-- | For skip samples data, the reason to skip the samples
data SkipSamplesReason = SkipSamplesPadding | SkipSamplesConvergence deriving (Eq, Show)

instance Storable SkipSamplesReason where
	sizeOf _ = 1
	alignment _ = 1
	peek ptr = do
		s <- peek (castPtr ptr :: Ptr Word8)
		if s == 0 then return SkipSamplesPadding else return SkipSamplesConvergence
	poke ptr SkipSamplesPadding = poke (castPtr ptr) (0 :: Word8)
	poke ptr SkipSamplesConvergence = poke (castPtr ptr) (1 :: Word8)

-- | Skip samples data
data SkipSamples = SkipSamples {
	skipSamples_atStart :: Word32,
	skipSamples_atEnd :: Word32,
	skipSamples_startReason :: SkipSamplesReason,
	skipSamples_endReason :: SkipSamplesReason
}

instance Storable SkipSamples where
	sizeOf _ = 32 + 32 + 2
	alignment _ = 8
	peek ptr = do
		_atStart <- peek (castPtr ptr)
		_atEnd <- peek (ptr `plusPtr` 4)
		_startReason <- peek (ptr `plusPtr` 8)
		_endReason <- peek (ptr `plusPtr` 9)
		return$ SkipSamples
			_atStart _atEnd
			_startReason _endReason
	poke ptr (ss) = do
		poke (castPtr ptr) (skipSamples_atStart ss)
		poke (ptr `plusPtr` 4) (skipSamples_atEnd ss)
		poke (ptr `plusPtr` 8) (skipSamples_startReason ss)
		poke (ptr `plusPtr` 9) (skipSamples_endReason ss)

