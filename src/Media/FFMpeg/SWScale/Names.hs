module Media.FFMpeg.SWScale.Names (
	sws_sws_flags,
	sws_srcw,
	sws_srch,
	sws_dstw,
	sws_dsth,
	sws_src_format,
	sws_dst_format,
	sws_src_range,
	sws_dst_range,
	sws_param0,
	sws_param1,
	sws_src_v_chr_pos,
	sws_src_h_chr_pos,
	sws_dst_v_chr_pos,
	sws_dst_h_chr_pos,
	sws_sws_dither
) where

import Data.Int
import Foreign.C.Types
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Core
import Media.FFMpeg.Util

-- SwsContext:
-- ===========================

-- | Option "sws_flags" for SwsContext of type AVOptTypeFlags-sws_flags.
-- scaler flags
-- default value is AVOptionFlags 4.
sws_sws_flags :: OptionName SwsContext CInt
sws_sws_flags = OptionName "sws_flags"

-- | Option "srcw" for SwsContext of type AVOptTypeInt.
-- source width
-- default value is AVOptionInt 16.
sws_srcw :: OptionName SwsContext CInt
sws_srcw = OptionName "srcw"

-- | Option "srch" for SwsContext of type AVOptTypeInt.
-- source height
-- default value is AVOptionInt 16.
sws_srch :: OptionName SwsContext CInt
sws_srch = OptionName "srch"

-- | Option "dstw" for SwsContext of type AVOptTypeInt.
-- destination width
-- default value is AVOptionInt 16.
sws_dstw :: OptionName SwsContext CInt
sws_dstw = OptionName "dstw"

-- | Option "dsth" for SwsContext of type AVOptTypeInt.
-- destination height
-- default value is AVOptionInt 16.
sws_dsth :: OptionName SwsContext CInt
sws_dsth = OptionName "dsth"

-- | Option "src_format" for SwsContext of type AVOptTypeInt.
-- source format
-- default value is AVOptionInt 0.
sws_src_format :: OptionName SwsContext CInt
sws_src_format = OptionName "src_format"

-- | Option "dst_format" for SwsContext of type AVOptTypeInt.
-- destination format
-- default value is AVOptionInt 0.
sws_dst_format :: OptionName SwsContext CInt
sws_dst_format = OptionName "dst_format"

-- | Option "src_range" for SwsContext of type AVOptTypeInt.
-- source range
-- default value is AVOptionInt 0.
sws_src_range :: OptionName SwsContext CInt
sws_src_range = OptionName "src_range"

-- | Option "dst_range" for SwsContext of type AVOptTypeInt.
-- destination range
-- default value is AVOptionInt 0.
sws_dst_range :: OptionName SwsContext CInt
sws_dst_range = OptionName "dst_range"

-- | Option "param0" for SwsContext of type AVOptTypeDouble.
-- scaler param 0
-- default value is AVOptionDouble 123456.0.
sws_param0 :: OptionName SwsContext Double
sws_param0 = OptionName "param0"

-- | Option "param1" for SwsContext of type AVOptTypeDouble.
-- scaler param 1
-- default value is AVOptionDouble 123456.0.
sws_param1 :: OptionName SwsContext Double
sws_param1 = OptionName "param1"

-- | Option "src_v_chr_pos" for SwsContext of type AVOptTypeInt.
-- source vertical chroma position in luma grid/256
-- default value is AVOptionInt (-513).
sws_src_v_chr_pos :: OptionName SwsContext CInt
sws_src_v_chr_pos = OptionName "src_v_chr_pos"

-- | Option "src_h_chr_pos" for SwsContext of type AVOptTypeInt.
-- source horizontal chroma position in luma grid/256
-- default value is AVOptionInt (-513).
sws_src_h_chr_pos :: OptionName SwsContext CInt
sws_src_h_chr_pos = OptionName "src_h_chr_pos"

-- | Option "dst_v_chr_pos" for SwsContext of type AVOptTypeInt.
-- destination vertical chroma position in luma grid/256
-- default value is AVOptionInt (-513).
sws_dst_v_chr_pos :: OptionName SwsContext CInt
sws_dst_v_chr_pos = OptionName "dst_v_chr_pos"

-- | Option "dst_h_chr_pos" for SwsContext of type AVOptTypeInt.
-- destination horizontal chroma position in luma grid/256
-- default value is AVOptionInt (-513).
sws_dst_h_chr_pos :: OptionName SwsContext CInt
sws_dst_h_chr_pos = OptionName "dst_h_chr_pos"

-- | Option "sws_dither" for SwsContext of type AVOptTypeInt-sws_dither.
-- set dithering algorithm
-- default value is AVOptionInt 1.
sws_sws_dither :: OptionName SwsContext CInt
sws_sws_dither = OptionName "sws_dither"

