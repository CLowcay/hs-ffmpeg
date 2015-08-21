{- |
 
Description : Named options for libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Named options for libavcodec

-}

module Media.FFMpeg.Codec.Names (
	codec_b,
	codec_ab,
	codec_bt,
	codec_flags,
	codec_me_method,
	codec_time_base,
	codec_g,
	codec_ar,
	codec_ac,
	codec_cutoff,
	codec_frame_size,
	codec_frame_number,
	codec_delay,
	codec_qcomp,
	codec_qblur,
	codec_qmin,
	codec_qmax,
	codec_qdiff,
	codec_bf,
	codec_b_qfactor,
	codec_rc_strategy,
	codec_b_strategy,
	codec_ps,
	codec_mv_bits,
	codec_header_bits,
	codec_i_tex_bits,
	codec_p_tex_bits,
	codec_i_count,
	codec_p_count,
	codec_skip_count,
	codec_misc_bits,
	codec_frame_bits,
	codec_codec_tag,
	codec_bug,
	codec_strict,
	codec_b_qoffset,
	codec_err_detect,
	codec_has_b_frames,
	codec_block_align,
	codec_mpeg_quant,
	codec_qsquish,
	codec_rc_qmod_amp,
	codec_rc_qmod_freq,
	codec_rc_override_count,
	codec_rc_eq,
	codec_maxrate,
	codec_minrate,
	codec_bufsize,
	codec_rc_buf_aggressivity,
	codec_i_qfactor,
	codec_i_qoffset,
	codec_rc_init_cplx,
	codec_dct,
	codec_lumi_mask,
	codec_tcplx_mask,
	codec_scplx_mask,
	codec_p_mask,
	codec_dark_mask,
	codec_idct,
	codec_slice_count,
	codec_ec,
	codec_bits_per_coded_sample,
	codec_pred,
	codec_aspect,
	codec_debug,
	codec_vismv,
	codec_cmp,
	codec_subcmp,
	codec_mbcmp,
	codec_ildctcmp,
	codec_dia_size,
	codec_last_pred,
	codec_preme,
	codec_precmp,
	codec_pre_dia_size,
	codec_subq,
	codec_dtg_active_format,
	codec_me_range,
	codec_ibias,
	codec_pbias,
	codec_global_quality,
	codec_coder,
	codec_context,
	codec_slice_flags,
	codec_xvmc_acceleration,
	codec_mbd,
	codec_stream_codec_tag,
	codec_sc_threshold,
	codec_lmin,
	codec_lmax,
	codec_nr,
	codec_rc_init_occupancy,
	codec_flags2,
	codec_error,
	codec_threads,
	codec_me_threshold,
	codec_mb_threshold,
	codec_dc,
	codec_nssew,
	codec_skip_top,
	codec_skip_bottom,
	codec_profile,
	codec_level,
	codec_lowres,
	codec_skip_threshold,
	codec_skip_factor,
	codec_skip_exp,
	codec_skipcmp,
	codec_border_mask,
	codec_mblmin,
	codec_mblmax,
	codec_mepc,
	codec_skip_loop_filter,
	codec_skip_idct,
	codec_skip_frame,
	codec_bidir_refine,
	codec_brd_scale,
	codec_keyint_min,
	codec_refs,
	codec_chromaoffset,
	codec_trellis,
	codec_sc_factor,
	codec_mv0_threshold,
	codec_b_sensitivity,
	codec_compression_level,
	codec_min_prediction_order,
	codec_max_prediction_order,
	codec_timecode_frame_start,
	codec_request_channels,
	codec_bits_per_raw_sample,
	codec_channel_layout,
	codec_request_channel_layout,
	codec_rc_max_vbv_use,
	codec_rc_min_vbv_use,
	codec_ticks_per_frame,
	codec_color_primaries,
	codec_color_trc,
	codec_colorspace,
	codec_color_range,
	codec_chroma_sample_location,
	codec_log_level_offset,
	codec_slices,
	codec_thread_type,
	codec_audio_service_type,
	codec_request_sample_fmt,
	codec_pkt_timebase,
	codec_sub_charenc,
	codec_sub_charenc_mode,
	codec_refcounted_frames,
	codec_side_data_only_packets,
	codec_skip_alpha,
	codec_field_order,
	codec_dump_separator,
	codec_codec_whitelist,
	codec_pixel_format,
	codec_video_size,
	frame_best_effort_timestamp,
	frame_pkt_pos,
	frame_pkt_size,
	frame_sample_aspect_ratio,
	frame_width,
	frame_height,
	frame_format,
	frame_channel_layout,
	frame_sample_rate,
	subtitle_x,
	subtitle_y,
	subtitle_w,
	subtitle_h,
	subtitle_type,
	subtitle_flags,
	subtitle_forced
) where

import Data.Int
import Foreign.C.Types
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- AVCodecContext:
-- ===========================

-- | Option "b" for AVCodecContext of type AVOptTypeInt.
-- set bitrate (in bits/s)
-- default value is AVOptionInt 200000.
codec_b :: OptionName AVCodecContext CInt
codec_b = OptionName "b"

-- | Option "ab" for AVCodecContext of type AVOptTypeInt.
-- set bitrate (in bits/s)
-- default value is AVOptionInt 128000.
codec_ab :: OptionName AVCodecContext CInt
codec_ab = OptionName "ab"

-- | Option "bt" for AVCodecContext of type AVOptTypeInt.
-- Set video bitrate tolerance (in bits/s). In 1-pass mode, bitrate tolerance
-- specifies how far ratecontrol is willing to deviate from the target average
-- bitrate value. This is not related to minimum/maximum bitrate. Lowering
-- tolerance too much has an adverse effect on quality.
-- default value is AVOptionInt 4000000.
codec_bt :: OptionName AVCodecContext CInt
codec_bt = OptionName "bt"

-- | Option "flags" for AVCodecContext of type AVOptTypeFlags-flags.
-- default value is AVOptionFlags 0.
codec_flags :: OptionName AVCodecContext AVCodecFlag
codec_flags = OptionName "flags"

-- | Option "me_method" for AVCodecContext of type AVOptTypeInt-me_method.
-- set motion estimation method
-- default value is AVOptionInt 5.
codec_me_method :: OptionName AVCodecContext Motion_Est_ID
codec_me_method = OptionName "me_method"

-- | Option "time_base" for AVCodecContext of type AVOptTypeRational.
codec_time_base :: OptionName AVCodecContext AVRational
codec_time_base = OptionName "time_base"

-- | Option "g" for AVCodecContext of type AVOptTypeInt.
-- set the group of picture (GOP) size
-- default value is AVOptionInt 12.
codec_g :: OptionName AVCodecContext CInt
codec_g = OptionName "g"

-- | Option "ar" for AVCodecContext of type AVOptTypeInt.
-- set audio sampling rate (in Hz)
-- default value is AVOptionInt 0.
codec_ar :: OptionName AVCodecContext CInt
codec_ar = OptionName "ar"

-- | Option "ac" for AVCodecContext of type AVOptTypeInt.
-- set number of audio channels
-- default value is AVOptionInt 0.
codec_ac :: OptionName AVCodecContext CInt
codec_ac = OptionName "ac"

-- | Option "cutoff" for AVCodecContext of type AVOptTypeInt.
-- set cutoff bandwidth
-- default value is AVOptionInt 0.
codec_cutoff :: OptionName AVCodecContext CInt
codec_cutoff = OptionName "cutoff"

-- | Option "frame_size" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_frame_size :: OptionName AVCodecContext CInt
codec_frame_size = OptionName "frame_size"

-- | Option "frame_number" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_frame_number :: OptionName AVCodecContext CInt
codec_frame_number = OptionName "frame_number"

-- | Option "delay" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_delay :: OptionName AVCodecContext CInt
codec_delay = OptionName "delay"

-- | Option "qcomp" for AVCodecContext of type AVOptTypeFloat.
-- video quantizer scale compression (VBR). Constant of ratecontrol equation.
-- Recommended range for default rc_eq: 0.0-1.0
-- default value is AVOptionFloat 0.0.
codec_qcomp :: OptionName AVCodecContext Float
codec_qcomp = OptionName "qcomp"

-- | Option "qblur" for AVCodecContext of type AVOptTypeFloat.
-- video quantizer scale blur (VBR)
-- default value is AVOptionFloat 0.0.
codec_qblur :: OptionName AVCodecContext Float
codec_qblur = OptionName "qblur"

-- | Option "qmin" for AVCodecContext of type AVOptTypeInt.
-- minimum video quantizer scale (VBR)
-- default value is AVOptionInt 2.
codec_qmin :: OptionName AVCodecContext CInt
codec_qmin = OptionName "qmin"

-- | Option "qmax" for AVCodecContext of type AVOptTypeInt.
-- maximum video quantizer scale (VBR)
-- default value is AVOptionInt 31.
codec_qmax :: OptionName AVCodecContext CInt
codec_qmax = OptionName "qmax"

-- | Option "qdiff" for AVCodecContext of type AVOptTypeInt.
-- maximum difference between the quantizer scales (VBR)
-- default value is AVOptionInt 3.
codec_qdiff :: OptionName AVCodecContext CInt
codec_qdiff = OptionName "qdiff"

-- | Option "bf" for AVCodecContext of type AVOptTypeInt.
-- set maximum number of B frames between non-B-frames
-- default value is AVOptionInt 0.
codec_bf :: OptionName AVCodecContext CInt
codec_bf = OptionName "bf"

-- | Option "b_qfactor" for AVCodecContext of type AVOptTypeFloat.
-- QP factor between P- and B-frames
-- default value is AVOptionFloat 0.0.
codec_b_qfactor :: OptionName AVCodecContext Float
codec_b_qfactor = OptionName "b_qfactor"

-- | Option "rc_strategy" for AVCodecContext of type AVOptTypeInt.
-- ratecontrol method
-- default value is AVOptionInt 0.
codec_rc_strategy :: OptionName AVCodecContext CInt
codec_rc_strategy = OptionName "rc_strategy"

-- | Option "b_strategy" for AVCodecContext of type AVOptTypeInt.
-- strategy to choose between I/P/B-frames
-- default value is AVOptionInt 0.
codec_b_strategy :: OptionName AVCodecContext CInt
codec_b_strategy = OptionName "b_strategy"

-- | Option "ps" for AVCodecContext of type AVOptTypeInt.
-- RTP payload size in bytes
-- default value is AVOptionInt 0.
codec_ps :: OptionName AVCodecContext CInt
codec_ps = OptionName "ps"

-- | Option "mv_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_mv_bits :: OptionName AVCodecContext CInt
codec_mv_bits = OptionName "mv_bits"

-- | Option "header_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_header_bits :: OptionName AVCodecContext CInt
codec_header_bits = OptionName "header_bits"

-- | Option "i_tex_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_i_tex_bits :: OptionName AVCodecContext CInt
codec_i_tex_bits = OptionName "i_tex_bits"

-- | Option "p_tex_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_p_tex_bits :: OptionName AVCodecContext CInt
codec_p_tex_bits = OptionName "p_tex_bits"

-- | Option "i_count" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_i_count :: OptionName AVCodecContext CInt
codec_i_count = OptionName "i_count"

-- | Option "p_count" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_p_count :: OptionName AVCodecContext CInt
codec_p_count = OptionName "p_count"

-- | Option "skip_count" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_skip_count :: OptionName AVCodecContext CInt
codec_skip_count = OptionName "skip_count"

-- | Option "misc_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_misc_bits :: OptionName AVCodecContext CInt
codec_misc_bits = OptionName "misc_bits"

-- | Option "frame_bits" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_frame_bits :: OptionName AVCodecContext CInt
codec_frame_bits = OptionName "frame_bits"

-- | Option "codec_tag" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_codec_tag :: OptionName AVCodecContext CInt
codec_codec_tag = OptionName "codec_tag"

-- | Option "bug" for AVCodecContext of type AVOptTypeFlags-bug.
-- work around not autodetected encoder bugs
-- default value is AVOptionFlags 1.
codec_bug :: OptionName AVCodecContext FFBug
codec_bug = OptionName "bug"

-- | Option "strict" for AVCodecContext of type AVOptTypeInt-strict.
-- how strictly to follow the standards
-- default value is AVOptionInt 0.
codec_strict :: OptionName AVCodecContext FFCompliance
codec_strict = OptionName "strict"

-- | Option "b_qoffset" for AVCodecContext of type AVOptTypeFloat.
-- QP offset between P- and B-frames
-- default value is AVOptionFloat 0.0.
codec_b_qoffset :: OptionName AVCodecContext Float
codec_b_qoffset = OptionName "b_qoffset"

-- | Option "err_detect" for AVCodecContext of type AVOptTypeFlags-err_detect.
-- set error detection flags
-- default value is AVOptionFlags 0.
codec_err_detect :: OptionName AVCodecContext AVEF
codec_err_detect = OptionName "err_detect"

-- | Option "has_b_frames" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_has_b_frames :: OptionName AVCodecContext CInt
codec_has_b_frames = OptionName "has_b_frames"

-- | Option "block_align" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_block_align :: OptionName AVCodecContext CInt
codec_block_align = OptionName "block_align"

-- | Option "mpeg_quant" for AVCodecContext of type AVOptTypeInt.
-- use MPEG quantizers instead of H.263
-- default value is AVOptionInt 0.
codec_mpeg_quant :: OptionName AVCodecContext CInt
codec_mpeg_quant = OptionName "mpeg_quant"

-- | Option "qsquish" for AVCodecContext of type AVOptTypeFloat.
-- deprecated, use encoder private options instead
-- default value is AVOptionFloat 0.0.
codec_qsquish :: OptionName AVCodecContext Float
codec_qsquish = OptionName "qsquish"

-- | Option "rc_qmod_amp" for AVCodecContext of type AVOptTypeFloat.
-- deprecated, use encoder private options instead
-- default value is AVOptionFloat 0.0.
codec_rc_qmod_amp :: OptionName AVCodecContext Float
codec_rc_qmod_amp = OptionName "rc_qmod_amp"

-- | Option "rc_qmod_freq" for AVCodecContext of type AVOptTypeInt.
-- deprecated, use encoder private options instead
-- default value is AVOptionInt 0.
codec_rc_qmod_freq :: OptionName AVCodecContext CInt
codec_rc_qmod_freq = OptionName "rc_qmod_freq"

-- | Option "rc_override_count" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_rc_override_count :: OptionName AVCodecContext CInt
codec_rc_override_count = OptionName "rc_override_count"

-- | Option "rc_eq" for AVCodecContext of type AVOptTypeString.
-- deprecated, use encoder private options instead
codec_rc_eq :: OptionName AVCodecContext String
codec_rc_eq = OptionName "rc_eq"

-- | Option "maxrate" for AVCodecContext of type AVOptTypeInt.
-- maximum bitrate (in bits/s). Used for VBV together with bufsize.
-- default value is AVOptionInt 0.
codec_maxrate :: OptionName AVCodecContext CInt
codec_maxrate = OptionName "maxrate"

-- | Option "minrate" for AVCodecContext of type AVOptTypeInt.
-- minimum bitrate (in bits/s). Most useful in setting up a CBR encode. It is
-- of little use otherwise.
-- default value is AVOptionInt 0.
codec_minrate :: OptionName AVCodecContext CInt
codec_minrate = OptionName "minrate"

-- | Option "bufsize" for AVCodecContext of type AVOptTypeInt.
-- set ratecontrol buffer size (in bits)
-- default value is AVOptionInt 0.
codec_bufsize :: OptionName AVCodecContext CInt
codec_bufsize = OptionName "bufsize"

-- | Option "rc_buf_aggressivity" for AVCodecContext of type AVOptTypeFloat.
-- deprecated, use encoder private options instead
-- default value is AVOptionFloat 0.0.
codec_rc_buf_aggressivity :: OptionName AVCodecContext Float
codec_rc_buf_aggressivity = OptionName "rc_buf_aggressivity"

-- | Option "i_qfactor" for AVCodecContext of type AVOptTypeFloat.
-- QP factor between P- and I-frames
-- default value is AVOptionFloat (-1.5881868e-23).
codec_i_qfactor :: OptionName AVCodecContext Float
codec_i_qfactor = OptionName "i_qfactor"

-- | Option "i_qoffset" for AVCodecContext of type AVOptTypeFloat.
-- QP offset between P- and I-frames
-- default value is AVOptionFloat 0.0.
codec_i_qoffset :: OptionName AVCodecContext Float
codec_i_qoffset = OptionName "i_qoffset"

-- | Option "rc_init_cplx" for AVCodecContext of type AVOptTypeFloat.
-- deprecated, use encoder private options instead
-- default value is AVOptionFloat 0.0.
codec_rc_init_cplx :: OptionName AVCodecContext Float
codec_rc_init_cplx = OptionName "rc_init_cplx"

-- | Option "dct" for AVCodecContext of type AVOptTypeInt-dct.
-- DCT algorithm
-- default value is AVOptionInt 0.
codec_dct :: OptionName AVCodecContext FFDCT
codec_dct = OptionName "dct"

-- | Option "lumi_mask" for AVCodecContext of type AVOptTypeFloat.
-- compresses bright areas stronger than medium ones
-- default value is AVOptionFloat 0.0.
codec_lumi_mask :: OptionName AVCodecContext Float
codec_lumi_mask = OptionName "lumi_mask"

-- | Option "tcplx_mask" for AVCodecContext of type AVOptTypeFloat.
-- temporal complexity masking
-- default value is AVOptionFloat 0.0.
codec_tcplx_mask :: OptionName AVCodecContext Float
codec_tcplx_mask = OptionName "tcplx_mask"

-- | Option "scplx_mask" for AVCodecContext of type AVOptTypeFloat.
-- spatial complexity masking
-- default value is AVOptionFloat 0.0.
codec_scplx_mask :: OptionName AVCodecContext Float
codec_scplx_mask = OptionName "scplx_mask"

-- | Option "p_mask" for AVCodecContext of type AVOptTypeFloat.
-- inter masking
-- default value is AVOptionFloat 0.0.
codec_p_mask :: OptionName AVCodecContext Float
codec_p_mask = OptionName "p_mask"

-- | Option "dark_mask" for AVCodecContext of type AVOptTypeFloat.
-- compresses dark areas stronger than medium ones
-- default value is AVOptionFloat 0.0.
codec_dark_mask :: OptionName AVCodecContext Float
codec_dark_mask = OptionName "dark_mask"

-- | Option "idct" for AVCodecContext of type AVOptTypeInt-idct.
-- select IDCT implementation
-- default value is AVOptionInt 0.
codec_idct :: OptionName AVCodecContext FFIdct
codec_idct = OptionName "idct"

-- | Option "slice_count" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_slice_count :: OptionName AVCodecContext CInt
codec_slice_count = OptionName "slice_count"

-- | Option "ec" for AVCodecContext of type AVOptTypeFlags-ec.
-- set error concealment strategy
-- default value is AVOptionFlags 3.
codec_ec :: OptionName AVCodecContext FFEC
codec_ec = OptionName "ec"

-- | Option "bits_per_coded_sample" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_bits_per_coded_sample :: OptionName AVCodecContext CInt
codec_bits_per_coded_sample = OptionName "bits_per_coded_sample"

-- | Option "pred" for AVCodecContext of type AVOptTypeInt-pred.
-- prediction method
-- default value is AVOptionInt 0.
codec_pred :: OptionName AVCodecContext FFPred
codec_pred = OptionName "pred"

-- | Option "aspect" for AVCodecContext of type AVOptTypeRational.
-- sample aspect ratio
codec_aspect :: OptionName AVCodecContext AVRational
codec_aspect = OptionName "aspect"

-- | Option "debug" for AVCodecContext of type AVOptTypeFlags-debug.
-- print specific debug info
-- default value is AVOptionFlags 0.
codec_debug :: OptionName AVCodecContext FFDebug
codec_debug = OptionName "debug"

-- | Option "vismv" for AVCodecContext of type AVOptTypeFlags-debug_mv.
-- visualize motion vectors (MVs) (deprecated)
-- default value is AVOptionFlags 0.
codec_vismv :: OptionName AVCodecContext CInt
codec_vismv = OptionName "vismv"

-- | Option "cmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- full-pel ME compare function
-- default value is AVOptionInt 0.
codec_cmp :: OptionName AVCodecContext CInt
codec_cmp = OptionName "cmp"

-- | Option "subcmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- sub-pel ME compare function
-- default value is AVOptionInt 0.
codec_subcmp :: OptionName AVCodecContext CInt
codec_subcmp = OptionName "subcmp"

-- | Option "mbcmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- macroblock compare function
-- default value is AVOptionInt 0.
codec_mbcmp :: OptionName AVCodecContext CInt
codec_mbcmp = OptionName "mbcmp"

-- | Option "ildctcmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- interlaced DCT compare function
-- default value is AVOptionInt 8.
codec_ildctcmp :: OptionName AVCodecContext CInt
codec_ildctcmp = OptionName "ildctcmp"

-- | Option "dia_size" for AVCodecContext of type AVOptTypeInt.
-- diamond type & size for motion estimation
-- default value is AVOptionInt 0.
codec_dia_size :: OptionName AVCodecContext CInt
codec_dia_size = OptionName "dia_size"

-- | Option "last_pred" for AVCodecContext of type AVOptTypeInt.
-- amount of motion predictors from the previous frame
-- default value is AVOptionInt 0.
codec_last_pred :: OptionName AVCodecContext CInt
codec_last_pred = OptionName "last_pred"

-- | Option "preme" for AVCodecContext of type AVOptTypeInt.
-- pre motion estimation
-- default value is AVOptionInt 0.
codec_preme :: OptionName AVCodecContext CInt
codec_preme = OptionName "preme"

-- | Option "precmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- pre motion estimation compare function
-- default value is AVOptionInt 0.
codec_precmp :: OptionName AVCodecContext CInt
codec_precmp = OptionName "precmp"

-- | Option "pre_dia_size" for AVCodecContext of type AVOptTypeInt.
-- diamond type & size for motion estimation pre-pass
-- default value is AVOptionInt 0.
codec_pre_dia_size :: OptionName AVCodecContext CInt
codec_pre_dia_size = OptionName "pre_dia_size"

-- | Option "subq" for AVCodecContext of type AVOptTypeInt.
-- sub-pel motion estimation quality
-- default value is AVOptionInt 8.
codec_subq :: OptionName AVCodecContext CInt
codec_subq = OptionName "subq"

-- | Option "dtg_active_format" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_dtg_active_format :: OptionName AVCodecContext CInt
codec_dtg_active_format = OptionName "dtg_active_format"

-- | Option "me_range" for AVCodecContext of type AVOptTypeInt.
-- limit motion vectors range (1023 for DivX player)
-- default value is AVOptionInt 0.
codec_me_range :: OptionName AVCodecContext CInt
codec_me_range = OptionName "me_range"

-- | Option "ibias" for AVCodecContext of type AVOptTypeInt.
-- intra quant bias
-- default value is AVOptionInt 999999.
codec_ibias :: OptionName AVCodecContext CInt
codec_ibias = OptionName "ibias"

-- | Option "pbias" for AVCodecContext of type AVOptTypeInt.
-- inter quant bias
-- default value is AVOptionInt 999999.
codec_pbias :: OptionName AVCodecContext CInt
codec_pbias = OptionName "pbias"

-- | Option "global_quality" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_global_quality :: OptionName AVCodecContext CInt
codec_global_quality = OptionName "global_quality"

-- | Option "coder" for AVCodecContext of type AVOptTypeInt-coder.
-- default value is AVOptionInt 0.
codec_coder :: OptionName AVCodecContext FFCoderType
codec_coder = OptionName "coder"

-- | Option "context" for AVCodecContext of type AVOptTypeInt.
-- context model
-- default value is AVOptionInt 0.
codec_context :: OptionName AVCodecContext CInt
codec_context = OptionName "context"

-- | Option "slice_flags" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_slice_flags :: OptionName AVCodecContext CInt
codec_slice_flags = OptionName "slice_flags"

-- | Option "xvmc_acceleration" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_xvmc_acceleration :: OptionName AVCodecContext CInt
codec_xvmc_acceleration = OptionName "xvmc_acceleration"

-- | Option "mbd" for AVCodecContext of type AVOptTypeInt-mbd.
-- macroblock decision algorithm (high quality mode)
-- default value is AVOptionInt 0.
codec_mbd :: OptionName AVCodecContext FFMBDecision
codec_mbd = OptionName "mbd"

-- | Option "stream_codec_tag" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_stream_codec_tag :: OptionName AVCodecContext CInt
codec_stream_codec_tag = OptionName "stream_codec_tag"

-- | Option "sc_threshold" for AVCodecContext of type AVOptTypeInt.
-- scene change threshold
-- default value is AVOptionInt 0.
codec_sc_threshold :: OptionName AVCodecContext CInt
codec_sc_threshold = OptionName "sc_threshold"

-- | Option "lmin" for AVCodecContext of type AVOptTypeInt.
-- deprecated, use encoder private options instead
-- default value is AVOptionInt 0.
codec_lmin :: OptionName AVCodecContext CInt
codec_lmin = OptionName "lmin"

-- | Option "lmax" for AVCodecContext of type AVOptTypeInt.
-- deprecated, use encoder private options instead
-- default value is AVOptionInt 0.
codec_lmax :: OptionName AVCodecContext CInt
codec_lmax = OptionName "lmax"

-- | Option "nr" for AVCodecContext of type AVOptTypeInt.
-- noise reduction
-- default value is AVOptionInt 0.
codec_nr :: OptionName AVCodecContext CInt
codec_nr = OptionName "nr"

-- | Option "rc_init_occupancy" for AVCodecContext of type AVOptTypeInt.
-- number of bits which should be loaded into the rc buffer before decoding
-- starts
-- default value is AVOptionInt 0.
codec_rc_init_occupancy :: OptionName AVCodecContext CInt
codec_rc_init_occupancy = OptionName "rc_init_occupancy"

-- | Option "flags2" for AVCodecContext of type AVOptTypeFlags-flags2.
-- default value is AVOptionFlags 0.
codec_flags2 :: OptionName AVCodecContext AVCodecFlag2
codec_flags2 = OptionName "flags2"

-- | Option "error" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_error :: OptionName AVCodecContext CInt
codec_error = OptionName "error"

-- | Option "threads" for AVCodecContext of type AVOptTypeInt-threads.
-- default value is AVOptionInt 1.
codec_threads :: OptionName AVCodecContext CInt
codec_threads = OptionName "threads"

-- | Option "me_threshold" for AVCodecContext of type AVOptTypeInt.
-- motion estimation threshold
-- default value is AVOptionInt 0.
codec_me_threshold :: OptionName AVCodecContext CInt
codec_me_threshold = OptionName "me_threshold"

-- | Option "mb_threshold" for AVCodecContext of type AVOptTypeInt.
-- macroblock threshold
-- default value is AVOptionInt 0.
codec_mb_threshold :: OptionName AVCodecContext CInt
codec_mb_threshold = OptionName "mb_threshold"

-- | Option "dc" for AVCodecContext of type AVOptTypeInt.
-- intra_dc_precision
-- default value is AVOptionInt 0.
codec_dc :: OptionName AVCodecContext CInt
codec_dc = OptionName "dc"

-- | Option "nssew" for AVCodecContext of type AVOptTypeInt.
-- nsse weight
-- default value is AVOptionInt 8.
codec_nssew :: OptionName AVCodecContext CInt
codec_nssew = OptionName "nssew"

-- | Option "skip_top" for AVCodecContext of type AVOptTypeInt.
-- number of macroblock rows at the top which are skipped
-- default value is AVOptionInt 0.
codec_skip_top :: OptionName AVCodecContext CInt
codec_skip_top = OptionName "skip_top"

-- | Option "skip_bottom" for AVCodecContext of type AVOptTypeInt.
-- number of macroblock rows at the bottom which are skipped
-- default value is AVOptionInt 0.
codec_skip_bottom :: OptionName AVCodecContext CInt
codec_skip_bottom = OptionName "skip_bottom"

-- | Option "profile" for AVCodecContext of type AVOptTypeInt-profile.
-- default value is AVOptionInt (-99).
codec_profile :: OptionName AVCodecContext FFProfile
codec_profile = OptionName "profile"

-- | Option "level" for AVCodecContext of type AVOptTypeInt-level.
-- default value is AVOptionInt (-99).
codec_level :: OptionName AVCodecContext FFLevel
codec_level = OptionName "level"

-- | Option "lowres" for AVCodecContext of type AVOptTypeInt.
-- decode at 1= 1/2, 2=1/4, 3=1/8 resolutions
-- default value is AVOptionInt 0.
codec_lowres :: OptionName AVCodecContext CInt
codec_lowres = OptionName "lowres"

-- | Option "skip_threshold" for AVCodecContext of type AVOptTypeInt.
-- frame skip threshold
-- default value is AVOptionInt 0.
codec_skip_threshold :: OptionName AVCodecContext CInt
codec_skip_threshold = OptionName "skip_threshold"

-- | Option "skip_factor" for AVCodecContext of type AVOptTypeInt.
-- frame skip factor
-- default value is AVOptionInt 0.
codec_skip_factor :: OptionName AVCodecContext CInt
codec_skip_factor = OptionName "skip_factor"

-- | Option "skip_exp" for AVCodecContext of type AVOptTypeInt.
-- frame skip exponent
-- default value is AVOptionInt 0.
codec_skip_exp :: OptionName AVCodecContext CInt
codec_skip_exp = OptionName "skip_exp"

-- | Option "skipcmp" for AVCodecContext of type AVOptTypeInt-cmp_func.
-- frame skip compare function
-- default value is AVOptionInt 13.
codec_skipcmp :: OptionName AVCodecContext CInt
codec_skipcmp = OptionName "skipcmp"

-- | Option "border_mask" for AVCodecContext of type AVOptTypeFloat.
-- deprecated, use encoder private options instead
-- default value is AVOptionFloat 0.0.
codec_border_mask :: OptionName AVCodecContext Float
codec_border_mask = OptionName "border_mask"

-- | Option "mblmin" for AVCodecContext of type AVOptTypeInt.
-- minimum macroblock Lagrange factor (VBR)
-- default value is AVOptionInt 236.
codec_mblmin :: OptionName AVCodecContext CInt
codec_mblmin = OptionName "mblmin"

-- | Option "mblmax" for AVCodecContext of type AVOptTypeInt.
-- maximum macroblock Lagrange factor (VBR)
-- default value is AVOptionInt 3658.
codec_mblmax :: OptionName AVCodecContext CInt
codec_mblmax = OptionName "mblmax"

-- | Option "mepc" for AVCodecContext of type AVOptTypeInt.
-- motion estimation bitrate penalty compensation (1.0 = 256)
-- default value is AVOptionInt 256.
codec_mepc :: OptionName AVCodecContext CInt
codec_mepc = OptionName "mepc"

-- | Option "skip_loop_filter" for AVCodecContext of type AVOptTypeInt-avdiscard.
-- skip loop filtering process for the selected frames
-- default value is AVOptionInt 0.
codec_skip_loop_filter :: OptionName AVCodecContext CInt
codec_skip_loop_filter = OptionName "skip_loop_filter"

-- | Option "skip_idct" for AVCodecContext of type AVOptTypeInt-avdiscard.
-- skip IDCT/dequantization for the selected frames
-- default value is AVOptionInt 0.
codec_skip_idct :: OptionName AVCodecContext CInt
codec_skip_idct = OptionName "skip_idct"

-- | Option "skip_frame" for AVCodecContext of type AVOptTypeInt-avdiscard.
-- skip decoding for the selected frames
-- default value is AVOptionInt 0.
codec_skip_frame :: OptionName AVCodecContext CInt
codec_skip_frame = OptionName "skip_frame"

-- | Option "bidir_refine" for AVCodecContext of type AVOptTypeInt.
-- refine the two motion vectors used in bidirectional macroblocks
-- default value is AVOptionInt 1.
codec_bidir_refine :: OptionName AVCodecContext CInt
codec_bidir_refine = OptionName "bidir_refine"

-- | Option "brd_scale" for AVCodecContext of type AVOptTypeInt.
-- downscale frames for dynamic B-frame decision
-- default value is AVOptionInt 0.
codec_brd_scale :: OptionName AVCodecContext CInt
codec_brd_scale = OptionName "brd_scale"

-- | Option "keyint_min" for AVCodecContext of type AVOptTypeInt.
-- minimum interval between IDR-frames
-- default value is AVOptionInt 25.
codec_keyint_min :: OptionName AVCodecContext CInt
codec_keyint_min = OptionName "keyint_min"

-- | Option "refs" for AVCodecContext of type AVOptTypeInt.
-- reference frames to consider for motion compensation
-- default value is AVOptionInt 1.
codec_refs :: OptionName AVCodecContext CInt
codec_refs = OptionName "refs"

-- | Option "chromaoffset" for AVCodecContext of type AVOptTypeInt.
-- chroma QP offset from luma
-- default value is AVOptionInt 0.
codec_chromaoffset :: OptionName AVCodecContext CInt
codec_chromaoffset = OptionName "chromaoffset"

-- | Option "trellis" for AVCodecContext of type AVOptTypeInt.
-- rate-distortion optimal quantization
-- default value is AVOptionInt 0.
codec_trellis :: OptionName AVCodecContext CInt
codec_trellis = OptionName "trellis"

-- | Option "sc_factor" for AVCodecContext of type AVOptTypeInt.
-- multiplied by qscale for each frame and added to scene_change_score
-- default value is AVOptionInt 6.
codec_sc_factor :: OptionName AVCodecContext CInt
codec_sc_factor = OptionName "sc_factor"

-- | Option "mv0_threshold" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 256.
codec_mv0_threshold :: OptionName AVCodecContext CInt
codec_mv0_threshold = OptionName "mv0_threshold"

-- | Option "b_sensitivity" for AVCodecContext of type AVOptTypeInt.
-- adjust sensitivity of b_frame_strategy 1
-- default value is AVOptionInt 40.
codec_b_sensitivity :: OptionName AVCodecContext CInt
codec_b_sensitivity = OptionName "b_sensitivity"

-- | Option "compression_level" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt (-1).
codec_compression_level :: OptionName AVCodecContext CInt
codec_compression_level = OptionName "compression_level"

-- | Option "min_prediction_order" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt (-1).
codec_min_prediction_order :: OptionName AVCodecContext CInt
codec_min_prediction_order = OptionName "min_prediction_order"

-- | Option "max_prediction_order" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt (-1).
codec_max_prediction_order :: OptionName AVCodecContext CInt
codec_max_prediction_order = OptionName "max_prediction_order"

-- | Option "timecode_frame_start" for AVCodecContext of type AVOptTypeInt64.
-- GOP timecode frame start number, in non-drop-frame format
-- default value is AVOptionInt64 (-1).
codec_timecode_frame_start :: OptionName AVCodecContext Int64
codec_timecode_frame_start = OptionName "timecode_frame_start"

-- | Option "request_channels" for AVCodecContext of type AVOptTypeInt.
-- set desired number of audio channels
-- default value is AVOptionInt 0.
codec_request_channels :: OptionName AVCodecContext CInt
codec_request_channels = OptionName "request_channels"

-- | Option "bits_per_raw_sample" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_bits_per_raw_sample :: OptionName AVCodecContext CInt
codec_bits_per_raw_sample = OptionName "bits_per_raw_sample"

-- | Option "channel_layout" for AVCodecContext of type AVOptTypeInt64-channel_layout.
-- default value is AVOptionInt64 0.
codec_channel_layout :: OptionName AVCodecContext Int64
codec_channel_layout = OptionName "channel_layout"

-- | Option "request_channel_layout" for AVCodecContext of type AVOptTypeInt64-request_channel_layout.
-- default value is AVOptionInt64 0.
codec_request_channel_layout :: OptionName AVCodecContext Int64
codec_request_channel_layout = OptionName "request_channel_layout"

-- | Option "rc_max_vbv_use" for AVCodecContext of type AVOptTypeFloat.
-- default value is AVOptionFloat 0.0.
codec_rc_max_vbv_use :: OptionName AVCodecContext Float
codec_rc_max_vbv_use = OptionName "rc_max_vbv_use"

-- | Option "rc_min_vbv_use" for AVCodecContext of type AVOptTypeFloat.
-- default value is AVOptionFloat 0.0.
codec_rc_min_vbv_use :: OptionName AVCodecContext Float
codec_rc_min_vbv_use = OptionName "rc_min_vbv_use"

-- | Option "ticks_per_frame" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 1.
codec_ticks_per_frame :: OptionName AVCodecContext CInt
codec_ticks_per_frame = OptionName "ticks_per_frame"

-- | Option "color_primaries" for AVCodecContext of type AVOptTypeInt-color_primaries_type.
-- color primaries
-- default value is AVOptionInt 2.
codec_color_primaries :: OptionName AVCodecContext CInt
codec_color_primaries = OptionName "color_primaries"

-- | Option "color_trc" for AVCodecContext of type AVOptTypeInt-color_trc_type.
-- color transfer characteristics
-- default value is AVOptionInt 2.
codec_color_trc :: OptionName AVCodecContext CInt
codec_color_trc = OptionName "color_trc"

-- | Option "colorspace" for AVCodecContext of type AVOptTypeInt-colorspace_type.
-- color space
-- default value is AVOptionInt 2.
codec_colorspace :: OptionName AVCodecContext CInt
codec_colorspace = OptionName "colorspace"

-- | Option "color_range" for AVCodecContext of type AVOptTypeInt-color_range_type.
-- color range
-- default value is AVOptionInt 0.
codec_color_range :: OptionName AVCodecContext CInt
codec_color_range = OptionName "color_range"

-- | Option "chroma_sample_location" for AVCodecContext of type AVOptTypeInt-chroma_sample_location_type.
-- chroma sample location
-- default value is AVOptionInt 0.
codec_chroma_sample_location :: OptionName AVCodecContext CInt
codec_chroma_sample_location = OptionName "chroma_sample_location"

-- | Option "log_level_offset" for AVCodecContext of type AVOptTypeInt.
-- set the log level offset
-- default value is AVOptionInt 0.
codec_log_level_offset :: OptionName AVCodecContext CInt
codec_log_level_offset = OptionName "log_level_offset"

-- | Option "slices" for AVCodecContext of type AVOptTypeInt.
-- number of slices, used in parallelized encoding
-- default value is AVOptionInt 0.
codec_slices :: OptionName AVCodecContext CInt
codec_slices = OptionName "slices"

-- | Option "thread_type" for AVCodecContext of type AVOptTypeFlags-thread_type.
-- select multithreading type
-- default value is AVOptionFlags 3.
codec_thread_type :: OptionName AVCodecContext FFThread
codec_thread_type = OptionName "thread_type"

-- | Option "audio_service_type" for AVCodecContext of type AVOptTypeInt-audio_service_type.
-- audio service type
-- default value is AVOptionInt 0.
codec_audio_service_type :: OptionName AVCodecContext AVAudioServiceType
codec_audio_service_type = OptionName "audio_service_type"

-- | Option "request_sample_fmt" for AVCodecContext of type AVOptTypeSampleFmt-request_sample_fmt.
-- sample format audio decoders should prefer
-- default value is AVOptionSampleFormat (AVSampleFormat (-1)).
codec_request_sample_fmt :: OptionName AVCodecContext AVSampleFormat
codec_request_sample_fmt = OptionName "request_sample_fmt"

-- | Option "pkt_timebase" for AVCodecContext of type AVOptTypeRational.
codec_pkt_timebase :: OptionName AVCodecContext AVRational
codec_pkt_timebase = OptionName "pkt_timebase"

-- | Option "sub_charenc" for AVCodecContext of type AVOptTypeString.
-- set input text subtitles character encoding
codec_sub_charenc :: OptionName AVCodecContext String
codec_sub_charenc = OptionName "sub_charenc"

-- | Option "sub_charenc_mode" for AVCodecContext of type AVOptTypeFlags-sub_charenc_mode.
-- set input text subtitles character encoding mode
-- default value is AVOptionFlags 0.
codec_sub_charenc_mode :: OptionName AVCodecContext FFSubCharencMode
codec_sub_charenc_mode = OptionName "sub_charenc_mode"

-- | Option "refcounted_frames" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_refcounted_frames :: OptionName AVCodecContext CInt
codec_refcounted_frames = OptionName "refcounted_frames"

-- | Option "side_data_only_packets" for AVCodecContext of type AVOptTypeInt.
-- default value is AVOptionInt 0.
codec_side_data_only_packets :: OptionName AVCodecContext CInt
codec_side_data_only_packets = OptionName "side_data_only_packets"

-- | Option "skip_alpha" for AVCodecContext of type AVOptTypeInt.
-- Skip processing alpha
-- default value is AVOptionInt 0.
codec_skip_alpha :: OptionName AVCodecContext CInt
codec_skip_alpha = OptionName "skip_alpha"

-- | Option "field_order" for AVCodecContext of type AVOptTypeInt-field_order.
-- Field order
-- default value is AVOptionInt 0.
codec_field_order :: OptionName AVCodecContext AVFieldOrder
codec_field_order = OptionName "field_order"

-- | Option "dump_separator" for AVCodecContext of type AVOptTypeString.
-- set information dump field separator
codec_dump_separator :: OptionName AVCodecContext String
codec_dump_separator = OptionName "dump_separator"

-- | Option "codec_whitelist" for AVCodecContext of type AVOptTypeString.
-- List of decoders that are allowed to be used
codec_codec_whitelist :: OptionName AVCodecContext String
codec_codec_whitelist = OptionName "codec_whitelist"

-- | Option "pixel_format" for AVCodecContext of type AVOptTypePixelFmt.
-- set pixel format
-- default value is AVOptionPixelFormat (PixelFormat (-1)).
codec_pixel_format :: OptionName AVCodecContext PixelFormat
codec_pixel_format = OptionName "pixel_format"

-- | Option "video_size" for AVCodecContext of type AVOptTypeImageSize.
-- set video size
codec_video_size :: OptionName AVCodecContext ImageSize
codec_video_size = OptionName "video_size"

-- AVFrame:
-- ===========================

-- | Option "best_effort_timestamp" for AVFrame of type AVOptTypeInt64.
-- 
-- default value is AVOptionInt64 (-9223372036854775808).
frame_best_effort_timestamp :: OptionName AVFrame Int64
frame_best_effort_timestamp = OptionName "best_effort_timestamp"

-- | Option "pkt_pos" for AVFrame of type AVOptTypeInt64.
-- 
-- default value is AVOptionInt64 (-1).
frame_pkt_pos :: OptionName AVFrame Int64
frame_pkt_pos = OptionName "pkt_pos"

-- | Option "pkt_size" for AVFrame of type AVOptTypeInt64.
-- 
-- default value is AVOptionInt64 (-1).
frame_pkt_size :: OptionName AVFrame Int64
frame_pkt_size = OptionName "pkt_size"

-- | Option "sample_aspect_ratio" for AVFrame of type AVOptTypeRational.
-- 
frame_sample_aspect_ratio :: OptionName AVFrame AVRational
frame_sample_aspect_ratio = OptionName "sample_aspect_ratio"

-- | Option "width" for AVFrame of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
frame_width :: OptionName AVFrame CInt
frame_width = OptionName "width"

-- | Option "height" for AVFrame of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
frame_height :: OptionName AVFrame CInt
frame_height = OptionName "height"

-- | Option "format" for AVFrame of type AVOptTypeInt.
-- 
-- default value is AVOptionInt (-1).
frame_format :: OptionName AVFrame CInt
frame_format = OptionName "format"

-- | Option "channel_layout" for AVFrame of type AVOptTypeInt64.
-- 
-- default value is AVOptionInt64 0.
frame_channel_layout :: OptionName AVFrame Int64
frame_channel_layout = OptionName "channel_layout"

-- | Option "sample_rate" for AVFrame of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
frame_sample_rate :: OptionName AVFrame CInt
frame_sample_rate = OptionName "sample_rate"

-- AVSubtitleRect:
-- ===========================

-- | Option "x" for AVSubtitleRect of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
subtitle_x :: OptionName AVSubtitleRect CInt
subtitle_x = OptionName "x"

-- | Option "y" for AVSubtitleRect of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
subtitle_y :: OptionName AVSubtitleRect CInt
subtitle_y = OptionName "y"

-- | Option "w" for AVSubtitleRect of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
subtitle_w :: OptionName AVSubtitleRect CInt
subtitle_w = OptionName "w"

-- | Option "h" for AVSubtitleRect of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
subtitle_h :: OptionName AVSubtitleRect CInt
subtitle_h = OptionName "h"

-- | Option "type" for AVSubtitleRect of type AVOptTypeInt.
-- 
-- default value is AVOptionInt 0.
subtitle_type :: OptionName AVSubtitleRect CInt
subtitle_type = OptionName "type"

-- | Option "flags" for AVSubtitleRect of type AVOptTypeFlags-flags.
-- 
-- default value is AVOptionFlags 0.
subtitle_flags :: OptionName AVSubtitleRect CInt
subtitle_flags = OptionName "flags"

-- | Option "forced" for AVSubtitleRect of type AVOptTypeFlags.
-- 
-- default value is AVOptionFlags 0.
subtitle_forced :: OptionName AVSubtitleRect CInt
subtitle_forced = OptionName "forced"

