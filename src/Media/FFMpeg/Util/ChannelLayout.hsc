{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Description : Channel layout utility functions and structs
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil.

-}

module Media.FFMpeg.Util.ChannelLayout (
	AVChannel,
	av_ch_front_left,
	av_ch_front_right,
	av_ch_front_center,
	av_ch_low_frequency,
	av_ch_back_left,
	av_ch_back_right,
	av_ch_front_left_of_center,
	av_ch_front_right_of_center,
	av_ch_back_center,
	av_ch_side_left,
	av_ch_side_right,
	av_ch_top_center,
	av_ch_top_front_left,
	av_ch_top_front_center,
	av_ch_top_front_right,
	av_ch_top_back_left,
	av_ch_top_back_center,
	av_ch_top_back_right,
	av_ch_stereo_left,
	av_ch_stereo_right,
	av_ch_wide_left,
	av_ch_wide_right,
	av_ch_surround_direct_left,
	av_ch_surround_direct_right,
	av_ch_low_frequency_2,

	AVChannelLayout(..),
	av_ch_layout_native,
	av_ch_layout_mono,
	av_ch_layout_stereo,
	av_ch_layout_2point1,
	av_ch_layout_2_1,
	av_ch_layout_surround,
	av_ch_layout_3point1,
	av_ch_layout_4point0,
	av_ch_layout_4point1,
	av_ch_layout_2_2,
	av_ch_layout_quad,
	av_ch_layout_5point0,
	av_ch_layout_5point1,
	av_ch_layout_5point0_back,
	av_ch_layout_5point1_back,
	av_ch_layout_6point0,
	av_ch_layout_6point0_front,
	av_ch_layout_hexagonal,
	av_ch_layout_6point1,
	av_ch_layout_6point1_back,
	av_ch_layout_6point1_front,
	av_ch_layout_7point0,
	av_ch_layout_7point0_front,
	av_ch_layout_7point1,
	av_ch_layout_7point1_wide,
	av_ch_layout_7point1_wide_back,
	av_ch_layout_octagonal,
	av_ch_layout_stereo_downmix,

	getChannelLayoutChannels,

	AVMatrixEncoding,
	av_matrix_encoding_none,
	av_matrix_encoding_dolby,
	av_matrix_encoding_dplii,
	av_matrix_encoding_dpliix,
	av_matrix_encoding_dpliiz,
	av_matrix_encoding_dolbyex,
	av_matrix_encoding_dolbyheadphone,

	getChannelLayout,
	getChannelLayoutString,
	getChannelLayoutNbChannels,
	getDefaultChannelLayout,
	getChannelLayoutChannelIndex,
	channelLayoutExtractChannel,
	getChannelName,
	getChannelDescription,
	getStandardChannelLayout,
	standardChannelLayouts
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Internal.Common

-- | Flags that identify which channels are active in a channel layout
newtype AVChannel = AVChannel Word64 deriving (Eq, Show, CFlags)
#{enum AVChannel, AVChannel,
	av_ch_front_left = AV_CH_FRONT_LEFT,
	av_ch_front_right = AV_CH_FRONT_RIGHT,
	av_ch_front_center = AV_CH_FRONT_CENTER,
	av_ch_low_frequency = AV_CH_LOW_FREQUENCY,
	av_ch_back_left = AV_CH_BACK_LEFT,
	av_ch_back_right = AV_CH_BACK_RIGHT,
	av_ch_front_left_of_center = AV_CH_FRONT_LEFT_OF_CENTER,
	av_ch_front_right_of_center = AV_CH_FRONT_RIGHT_OF_CENTER,
	av_ch_back_center = AV_CH_BACK_CENTER,
	av_ch_side_left = AV_CH_SIDE_LEFT,
	av_ch_side_right = AV_CH_SIDE_RIGHT,
	av_ch_top_center = AV_CH_TOP_CENTER,
	av_ch_top_front_left = AV_CH_TOP_FRONT_LEFT,
	av_ch_top_front_center = AV_CH_TOP_FRONT_CENTER,
	av_ch_top_front_right = AV_CH_TOP_FRONT_RIGHT,
	av_ch_top_back_left = AV_CH_TOP_BACK_LEFT,
	av_ch_top_back_center = AV_CH_TOP_BACK_CENTER,
	av_ch_top_back_right = AV_CH_TOP_BACK_RIGHT,
	av_ch_stereo_left = AV_CH_STEREO_LEFT,
	av_ch_stereo_right = AV_CH_STEREO_RIGHT,
	av_ch_wide_left = AV_CH_WIDE_LEFT,
	av_ch_wide_right = AV_CH_WIDE_RIGHT,
	av_ch_surround_direct_left = AV_CH_SURROUND_DIRECT_LEFT,
	av_ch_surround_direct_right = AV_CH_SURROUND_DIRECT_RIGHT,
	av_ch_low_frequency_2 = AV_CH_LOW_FREQUENCY_2
}

-- | Channel layouts
newtype AVChannelLayout = AVChannelLayout Word64 deriving (Eq, Show, Storable)
#{enum AVChannelLayout, AVChannelLayout,
	av_ch_layout_native = AV_CH_LAYOUT_NATIVE,
	av_ch_layout_mono = AV_CH_LAYOUT_MONO,
	av_ch_layout_stereo = AV_CH_LAYOUT_STEREO,
	av_ch_layout_2point1 = AV_CH_LAYOUT_2POINT1,
	av_ch_layout_2_1 = AV_CH_LAYOUT_2_1,
	av_ch_layout_surround = AV_CH_LAYOUT_SURROUND,
	av_ch_layout_3point1 = AV_CH_LAYOUT_3POINT1,
	av_ch_layout_4point0 = AV_CH_LAYOUT_4POINT0,
	av_ch_layout_4point1 = AV_CH_LAYOUT_4POINT1,
	av_ch_layout_2_2 = AV_CH_LAYOUT_2_2,
	av_ch_layout_quad = AV_CH_LAYOUT_QUAD,
	av_ch_layout_5point0 = AV_CH_LAYOUT_5POINT0,
	av_ch_layout_5point1 = AV_CH_LAYOUT_5POINT1,
	av_ch_layout_5point0_back = AV_CH_LAYOUT_5POINT0_BACK,
	av_ch_layout_5point1_back = AV_CH_LAYOUT_5POINT1_BACK,
	av_ch_layout_6point0 = AV_CH_LAYOUT_6POINT0,
	av_ch_layout_6point0_front = AV_CH_LAYOUT_6POINT0_FRONT,
	av_ch_layout_hexagonal = AV_CH_LAYOUT_HEXAGONAL,
	av_ch_layout_6point1 = AV_CH_LAYOUT_6POINT1,
	av_ch_layout_6point1_back = AV_CH_LAYOUT_6POINT1_BACK,
	av_ch_layout_6point1_front = AV_CH_LAYOUT_6POINT1_FRONT,
	av_ch_layout_7point0 = AV_CH_LAYOUT_7POINT0,
	av_ch_layout_7point0_front = AV_CH_LAYOUT_7POINT0_FRONT,
	av_ch_layout_7point1 = AV_CH_LAYOUT_7POINT1,
	av_ch_layout_7point1_wide = AV_CH_LAYOUT_7POINT1_WIDE,
	av_ch_layout_7point1_wide_back = AV_CH_LAYOUT_7POINT1_WIDE_BACK,
	av_ch_layout_octagonal = AV_CH_LAYOUT_OCTAGONAL,
	av_ch_layout_stereo_downmix = AV_CH_LAYOUT_STEREO_DOWNMIX
}

-- | Get the channels that are active in a channel layout
getChannelLayoutChannels :: AVChannelLayout -> AVChannel
getChannelLayoutChannels (AVChannelLayout x) = AVChannel x

-- | AVMatrixEncoding enum
newtype AVMatrixEncoding = AVMatrixEncoding CInt deriving (Eq, Show, CEnum, Storable)
#{enum AVMatrixEncoding, AVMatrixEncoding,
	av_matrix_encoding_none = AV_MATRIX_ENCODING_NONE,
	av_matrix_encoding_dolby = AV_MATRIX_ENCODING_DOLBY,
	av_matrix_encoding_dplii = AV_MATRIX_ENCODING_DPLII,
	av_matrix_encoding_dpliix = AV_MATRIX_ENCODING_DPLIIX,
	av_matrix_encoding_dpliiz = AV_MATRIX_ENCODING_DPLIIZ,
	av_matrix_encoding_dolbyex = AV_MATRIX_ENCODING_DOLBYEX,
	av_matrix_encoding_dolbyheadphone = AV_MATRIX_ENCODING_DOLBYHEADPHONE
}

foreign import ccall "av_get_channel_layout" av_get_channel_layout :: CString -> IO AVChannelLayout
foreign import ccall "av_get_channel_layout_string" av_get_channel_layout_string :: CString -> CInt -> CInt -> AVChannelLayout -> IO ()
foreign import ccall "av_get_channel_layout_nb_channels" av_get_channel_layout_nb_channels :: AVChannelLayout -> CInt
foreign import ccall "av_get_default_channel_layout" av_get_default_channel_layout :: CInt -> AVChannelLayout
foreign import ccall "av_get_channel_layout_channel_index" av_get_channel_layout_channel_index :: AVChannelLayout -> AVChannel -> CInt
foreign import ccall "av_channel_layout_extract_channel" av_channel_layout_extract_channel :: AVChannelLayout -> CInt -> AVChannel
foreign import ccall "av_get_channel_name" av_get_channel_name :: AVChannel -> CString
foreign import ccall "av_get_channel_description" av_get_channel_description :: AVChannel -> CString
foreign import ccall "av_get_standard_channel_layout" av_get_standard_channel_layout :: CUInt -> Ptr AVChannelLayout -> Ptr CString -> IO CInt

-- | Get the channel layout associated with a string channel layout name
getChannelLayout :: String -> Maybe AVChannelLayout
getChannelLayout name = unsafePerformIO.withCString name$ \cname -> do
	r <- av_get_channel_layout cname
	if r == (AVChannelLayout 0) then return Nothing else return$ Just r
	-- safe because there are no observable side-effects

-- | Get a description of a channel layout
getChannelLayoutString :: Int -> AVChannelLayout -> String
getChannelLayoutString nbChannels layout =
	unsafePerformIO.allocaBytes 256$ \cbuff -> do
		av_get_channel_layout_string cbuff 255 (fromIntegral nbChannels) layout
		peekCString cbuff
		-- safe because there are no observable side effects

-- | Get the number of channels in a channel layout
getChannelLayoutNbChannels :: AVChannelLayout -> Int
getChannelLayoutNbChannels = fromIntegral.av_get_channel_layout_nb_channels

-- | Get the default channel layout given a certain number of channels
getDefaultChannelLayout :: Int -> AVChannelLayout
getDefaultChannelLayout = av_get_default_channel_layout.fromIntegral

-- | Get the index of a channel in a channel layout
getChannelLayoutChannelIndex :: (MonadError String m) =>
	AVChannelLayout -> AVChannel -> m Int
getChannelLayoutChannelIndex layout channel = do
	let r = av_get_channel_layout_channel_index layout channel
	if r < 0 then throwError$
		"getChannelLayoutChannelIndex failed with error code " ++ (show r)
	else return.fromIntegral$ r

-- | Get the channel at a particular index from a Channel layout
channelLayoutExtractChannel :: AVChannelLayout -> Int -> Maybe AVChannel
channelLayoutExtractChannel layout i =
	let r = av_channel_layout_extract_channel layout (fromIntegral i)
	in if r == AVChannel 0 then Nothing else Just r

-- | Get the name of a channel
getChannelName :: AVChannel -> Maybe String
getChannelName channel = unsafePerformIO$ do
	let r = av_get_channel_name channel
	if r == nullPtr then return Nothing else Just <$> peekCString r
	-- safe because there are no observable side effects

-- | Get the description of a channel
getChannelDescription :: AVChannel -> Maybe String
getChannelDescription c = unsafePerformIO$ do
	let r = av_get_channel_description c
	if r == nullPtr then return Nothing else Just <$> peekCString r
	-- safe because there are no observable side effects

-- | Get a channel layout with description from an internal table
getStandardChannelLayout :: Word -> Maybe (AVChannelLayout, String)
getStandardChannelLayout i = unsafePerformIO$    -- no observable side-effects
	alloca $ \playout -> do
	alloca $ \pbuff -> do
		r <- av_get_standard_channel_layout (fromIntegral i) playout pbuff
		if r < 0 then return Nothing else do
			rs <- peekCString =<< peek pbuff
			rl <- peek playout
			return.Just$ (rl, rs)


-- | Get all the standard channel layouts
standardChannelLayouts :: [(AVChannelLayout, String)]
standardChannelLayouts = catMaybes.takeWhile isJust$
	getStandardChannelLayout <$> [0..]

