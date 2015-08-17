module Media.FFMpeg.SWScale.Names (
	swsopt_sws_flags,
	swsopt_fast_bilinear,
	swsopt_bilinear,
	swsopt_bicubic,
	swsopt_experimental,
	swsopt_neighbor,
	swsopt_area,
	swsopt_bicublin,
	swsopt_gauss,
	swsopt_sinc,
	swsopt_lanczos,
	swsopt_spline,
	swsopt_print_info,
	swsopt_accurate_rnd,
	swsopt_full_chroma_int,
	swsopt_full_chroma_inp,
	swsopt_bitexact,
	swsopt_error_diffusion,
	swsopt_srcw,
	swsopt_srch,
	swsopt_dstw,
	swsopt_dsth,
	swsopt_src_format,
	swsopt_dst_format,
	swsopt_src_range,
	swsopt_dst_range,
	swsopt_param0,
	swsopt_param1,
	swsopt_src_v_chr_pos,
	swsopt_src_h_chr_pos,
	swsopt_dst_v_chr_pos,
	swsopt_dst_h_chr_pos,
	swsopt_sws_dither,
	swsopt_auto,
	swsopt_bayer,
	swsopt_ed,
	swsopt_a_dither,
	swsopt_x_dither
) where

import Foreign.C.Types
import Data.Int

import Media.FFMpeg.Internal.Common
import Media.FFMpeg.SWScale.Core
import Media.FFMpeg.Util

-- SwsContext:
-- ===========================

-- | Option "sws_flags" for SwsContext of type av_opt_type_flags-sws_flags.
-- scaler flags
-- default value is AVOptionFlags 4.
swsopt_sws_flags :: OptionName SwsContext CInt
swsopt_sws_flags = OptionName "sws_flags"

-- | Option "fast_bilinear" for SwsContext of type av_opt_type_flags-sws_flags.
-- fast bilinear
-- default value is AVOptionFlags 1.
swsopt_fast_bilinear :: OptionName SwsContext CInt
swsopt_fast_bilinear = OptionName "fast_bilinear"

-- | Option "bilinear" for SwsContext of type av_opt_type_flags-sws_flags.
-- bilinear
-- default value is AVOptionFlags 2.
swsopt_bilinear :: OptionName SwsContext CInt
swsopt_bilinear = OptionName "bilinear"

-- | Option "bicubic" for SwsContext of type av_opt_type_flags-sws_flags.
-- bicubic
-- default value is AVOptionFlags 4.
swsopt_bicubic :: OptionName SwsContext CInt
swsopt_bicubic = OptionName "bicubic"

-- | Option "experimental" for SwsContext of type av_opt_type_flags-sws_flags.
-- experimental
-- default value is AVOptionFlags 8.
swsopt_experimental :: OptionName SwsContext CInt
swsopt_experimental = OptionName "experimental"

-- | Option "neighbor" for SwsContext of type av_opt_type_flags-sws_flags.
-- nearest neighbor
-- default value is AVOptionFlags 16.
swsopt_neighbor :: OptionName SwsContext CInt
swsopt_neighbor = OptionName "neighbor"

-- | Option "area" for SwsContext of type av_opt_type_flags-sws_flags.
-- averaging area
-- default value is AVOptionFlags 32.
swsopt_area :: OptionName SwsContext CInt
swsopt_area = OptionName "area"

-- | Option "bicublin" for SwsContext of type av_opt_type_flags-sws_flags.
-- luma bicubic, chroma bilinear
-- default value is AVOptionFlags 64.
swsopt_bicublin :: OptionName SwsContext CInt
swsopt_bicublin = OptionName "bicublin"

-- | Option "gauss" for SwsContext of type av_opt_type_flags-sws_flags.
-- gaussian
-- default value is AVOptionFlags 128.
swsopt_gauss :: OptionName SwsContext CInt
swsopt_gauss = OptionName "gauss"

-- | Option "sinc" for SwsContext of type av_opt_type_flags-sws_flags.
-- sinc
-- default value is AVOptionFlags 256.
swsopt_sinc :: OptionName SwsContext CInt
swsopt_sinc = OptionName "sinc"

-- | Option "lanczos" for SwsContext of type av_opt_type_flags-sws_flags.
-- lanczos
-- default value is AVOptionFlags 512.
swsopt_lanczos :: OptionName SwsContext CInt
swsopt_lanczos = OptionName "lanczos"

-- | Option "spline" for SwsContext of type av_opt_type_flags-sws_flags.
-- natural bicubic spline
-- default value is AVOptionFlags 1024.
swsopt_spline :: OptionName SwsContext CInt
swsopt_spline = OptionName "spline"

-- | Option "print_info" for SwsContext of type av_opt_type_flags-sws_flags.
-- print info
-- default value is AVOptionFlags 4096.
swsopt_print_info :: OptionName SwsContext CInt
swsopt_print_info = OptionName "print_info"

-- | Option "accurate_rnd" for SwsContext of type av_opt_type_flags-sws_flags.
-- accurate rounding
-- default value is AVOptionFlags 262144.
swsopt_accurate_rnd :: OptionName SwsContext CInt
swsopt_accurate_rnd = OptionName "accurate_rnd"

-- | Option "full_chroma_int" for SwsContext of type av_opt_type_flags-sws_flags.
-- full chroma interpolation
-- default value is AVOptionFlags 8192.
swsopt_full_chroma_int :: OptionName SwsContext CInt
swsopt_full_chroma_int = OptionName "full_chroma_int"

-- | Option "full_chroma_inp" for SwsContext of type av_opt_type_flags-sws_flags.
-- full chroma input
-- default value is AVOptionFlags 16384.
swsopt_full_chroma_inp :: OptionName SwsContext CInt
swsopt_full_chroma_inp = OptionName "full_chroma_inp"

-- | Option "bitexact" for SwsContext of type av_opt_type_flags-sws_flags.
-- 
-- default value is AVOptionFlags 524288.
swsopt_bitexact :: OptionName SwsContext CInt
swsopt_bitexact = OptionName "bitexact"

-- | Option "error_diffusion" for SwsContext of type av_opt_type_flags-sws_flags.
-- error diffusion dither
-- default value is AVOptionFlags 8388608.
swsopt_error_diffusion :: OptionName SwsContext CInt
swsopt_error_diffusion = OptionName "error_diffusion"

-- | Option "srcw" for SwsContext of type av_opt_type_flags.
-- source width
-- default value is AVOptionFlags 16.
swsopt_srcw :: OptionName SwsContext CInt
swsopt_srcw = OptionName "srcw"

-- | Option "srch" for SwsContext of type av_opt_type_flags.
-- source height
-- default value is AVOptionFlags 16.
swsopt_srch :: OptionName SwsContext CInt
swsopt_srch = OptionName "srch"

-- | Option "dstw" for SwsContext of type av_opt_type_flags.
-- destination width
-- default value is AVOptionFlags 16.
swsopt_dstw :: OptionName SwsContext CInt
swsopt_dstw = OptionName "dstw"

-- | Option "dsth" for SwsContext of type av_opt_type_flags.
-- destination height
-- default value is AVOptionFlags 16.
swsopt_dsth :: OptionName SwsContext CInt
swsopt_dsth = OptionName "dsth"

-- | Option "src_format" for SwsContext of type av_opt_type_flags.
-- source format
-- default value is AVOptionFlags 0.
swsopt_src_format :: OptionName SwsContext CInt
swsopt_src_format = OptionName "src_format"

-- | Option "dst_format" for SwsContext of type av_opt_type_flags.
-- destination format
-- default value is AVOptionFlags 0.
swsopt_dst_format :: OptionName SwsContext CInt
swsopt_dst_format = OptionName "dst_format"

-- | Option "src_range" for SwsContext of type av_opt_type_flags.
-- source range
-- default value is AVOptionFlags 0.
swsopt_src_range :: OptionName SwsContext CInt
swsopt_src_range = OptionName "src_range"

-- | Option "dst_range" for SwsContext of type av_opt_type_flags.
-- destination range
-- default value is AVOptionFlags 0.
swsopt_dst_range :: OptionName SwsContext CInt
swsopt_dst_range = OptionName "dst_range"

-- | Option "param0" for SwsContext of type av_opt_type_flags.
-- scaler param 0
-- default value is AVOptionFlags 0.
swsopt_param0 :: OptionName SwsContext CInt
swsopt_param0 = OptionName "param0"

-- | Option "param1" for SwsContext of type av_opt_type_flags.
-- scaler param 1
-- default value is AVOptionFlags 0.
swsopt_param1 :: OptionName SwsContext CInt
swsopt_param1 = OptionName "param1"

-- | Option "src_v_chr_pos" for SwsContext of type av_opt_type_flags.
-- source vertical chroma position in luma grid/256
-- default value is AVOptionFlags (-513).
swsopt_src_v_chr_pos :: OptionName SwsContext CInt
swsopt_src_v_chr_pos = OptionName "src_v_chr_pos"

-- | Option "src_h_chr_pos" for SwsContext of type av_opt_type_flags.
-- source horizontal chroma position in luma grid/256
-- default value is AVOptionFlags (-513).
swsopt_src_h_chr_pos :: OptionName SwsContext CInt
swsopt_src_h_chr_pos = OptionName "src_h_chr_pos"

-- | Option "dst_v_chr_pos" for SwsContext of type av_opt_type_flags.
-- destination vertical chroma position in luma grid/256
-- default value is AVOptionFlags (-513).
swsopt_dst_v_chr_pos :: OptionName SwsContext CInt
swsopt_dst_v_chr_pos = OptionName "dst_v_chr_pos"

-- | Option "dst_h_chr_pos" for SwsContext of type av_opt_type_flags.
-- destination horizontal chroma position in luma grid/256
-- default value is AVOptionFlags (-513).
swsopt_dst_h_chr_pos :: OptionName SwsContext CInt
swsopt_dst_h_chr_pos = OptionName "dst_h_chr_pos"

-- | Option "sws_dither" for SwsContext of type av_opt_type_flags-sws_dither.
-- set dithering algorithm
-- default value is AVOptionFlags 1.
swsopt_sws_dither :: OptionName SwsContext CInt
swsopt_sws_dither = OptionName "sws_dither"

-- | Option "auto" for SwsContext of type av_opt_type_flags-sws_dither.
-- leave choice to sws
-- default value is AVOptionFlags 1.
swsopt_auto :: OptionName SwsContext CInt
swsopt_auto = OptionName "auto"

-- | Option "bayer" for SwsContext of type av_opt_type_flags-sws_dither.
-- bayer dither
-- default value is AVOptionFlags 2.
swsopt_bayer :: OptionName SwsContext CInt
swsopt_bayer = OptionName "bayer"

-- | Option "ed" for SwsContext of type av_opt_type_flags-sws_dither.
-- error diffusion
-- default value is AVOptionFlags 3.
swsopt_ed :: OptionName SwsContext CInt
swsopt_ed = OptionName "ed"

-- | Option "a_dither" for SwsContext of type av_opt_type_flags-sws_dither.
-- arithmetic addition dither
-- default value is AVOptionFlags 4.
swsopt_a_dither :: OptionName SwsContext CInt
swsopt_a_dither = OptionName "a_dither"

-- | Option "x_dither" for SwsContext of type av_opt_type_flags-sws_dither.
-- arithmetic xor dither
-- default value is AVOptionFlags 5.
swsopt_x_dither :: OptionName SwsContext CInt
swsopt_x_dither = OptionName "x_dither"

