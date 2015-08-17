module Media.FFMpeg.Codec.Names (
	codec_b,
	codec_ab,
	codec_bt,
	codec_flags,
	codec_unaligned,
	codec_mv4,
	codec_qpel,
	codec_loop,
	codec_qscale,
	codec_gmc,
	codec_mv0,
	codec_input_preserved,
	codec_pass1,
	codec_pass2,
	codec_gray,
	codec_emu_edge,
	codec_psnr,
	codec_truncated,
	codec_naq,
	codec_ildct,
	codec_low_delay,
	codec_global_header,
	codec_bitexact,
	codec_aic,
	codec_ilme,
	codec_cgop,
	codec_output_corrupt,
	codec_fast,
	codec_noout,
	codec_ignorecrop,
	codec_local_header,
	codec_chunks,
	codec_showall,
	codec_export_mvs,
	codec_skip_manual,
	codec_me_method,
	codec_zero,
	codec_full,
	codec_epzs,
	codec_esa,
	codec_tesa,
	codec_dia,
	codec_log,
	codec_phods,
	codec_x1,
	codec_hex,
	codec_umh,
	codec_iter,
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
	codec_autodetect,
	codec_old_msmpeg4,
	codec_xvid_ilace,
	codec_ump4,
	codec_no_padding,
	codec_amv,
	codec_ac_vlc,
	codec_qpel_chroma,
	codec_std_qpel,
	codec_qpel_chroma2,
	codec_direct_blocksize,
	codec_edge,
	codec_hpel_chroma,
	codec_dc_clip,
	codec_ms,
	codec_trunc,
	codec_strict,
	codec_very,
	codec_normal,
	codec_unofficial,
	codec_experimental,
	codec_b_qoffset,
	codec_err_detect,
	codec_crccheck,
	codec_bitstream,
	codec_buffer,
	codec_explode,
	codec_ignore_err,
	codec_careful,
	codec_compliant,
	codec_aggressive,
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
	codec_auto,
	codec_fastint,
	codec_int,
	codec_mmx,
	codec_altivec,
	codec_faan,
	codec_lumi_mask,
	codec_tcplx_mask,
	codec_scplx_mask,
	codec_p_mask,
	codec_dark_mask,
	codec_idct,
	codec_simple,
	codec_simplemmx,
	codec_arm,
	codec_sh4,
	codec_simplearm,
	codec_simplearmv5te,
	codec_simplearmv6,
	codec_simpleneon,
	codec_simplealpha,
	codec_ipp,
	codec_xvid,
	codec_xvidmmx,
	codec_faani,
	codec_simpleauto,
	codec_slice_count,
	codec_ec,
	codec_guess_mvs,
	codec_deblock,
	codec_favor_inter,
	codec_bits_per_coded_sample,
	codec_pred,
	codec_left,
	codec_plane,
	codec_median,
	codec_aspect,
	codec_debug,
	codec_pict,
	codec_rc,
	codec_mb_type,
	codec_qp,
	codec_mv,
	codec_dct_coeff,
	codec_skip,
	codec_startcode,
	codec_pts,
	codec_er,
	codec_mmco,
	codec_bugs,
	codec_vis_qp,
	codec_vis_mb_type,
	codec_buffers,
	codec_thread_ops,
	codec_nomc,
	codec_vismv,
	codec_pf,
	codec_bb,
	codec_cmp,
	codec_subcmp,
	codec_mbcmp,
	codec_ildctcmp,
	codec_dia_size,
	codec_last_pred,
	codec_preme,
	codec_precmp,
	codec_sad,
	codec_sse,
	codec_satd,
	codec_bit,
	codec_rd,
	codec_vsad,
	codec_vsse,
	codec_nsse,
	codec_w53,
	codec_w97,
	codec_dctmax,
	codec_chroma,
	codec_pre_dia_size,
	codec_subq,
	codec_dtg_active_format,
	codec_me_range,
	codec_ibias,
	codec_pbias,
	codec_global_quality,
	codec_coder,
	codec_vlc,
	codec_raw,
	codec_rle,
	codec_deflate,
	codec_context,
	codec_slice_flags,
	codec_xvmc_acceleration,
	codec_mbd,
	codec_bits,
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
	codec_unknown,
	codec_aac_main,
	codec_aac_low,
	codec_aac_ssr,
	codec_aac_ltp,
	codec_aac_he,
	codec_aac_he_v2,
	codec_aac_ld,
	codec_aac_eld,
	codec_mpeg2_aac_low,
	codec_mpeg2_aac_he,
	codec_dts,
	codec_dts_es,
	codec_dts_96_24,
	codec_dts_hd_hra,
	codec_dts_hd_ma,
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
	codec_none,
	codec_default,
	codec_noref,
	codec_bidir,
	codec_nokey,
	codec_nointra,
	codec_all,
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
	codec_bt709,
	codec_unspecified,
	codec_bt470m,
	codec_bt470bg,
	codec_smpte170m,
	codec_smpte240m,
	codec_film,
	codec_bt2020,
	codec_color_trc,
	codec_gamma22,
	codec_gamma28,
	codec_linear,
	codec_log_sqrt,
	codec_iec61966_2_4,
	codec_bt1361,
	codec_iec61966_2_1,
	codec_bt2020_10bit,
	codec_bt2020_12bit,
	codec_colorspace,
	codec_rgb,
	codec_fcc,
	codec_ycocg,
	codec_bt2020_ncl,
	codec_bt2020_cl,
	codec_color_range,
	codec_mpeg,
	codec_jpeg,
	codec_chroma_sample_location,
	codec_center,
	codec_topleft,
	codec_top,
	codec_bottomleft,
	codec_bottom,
	codec_log_level_offset,
	codec_slices,
	codec_thread_type,
	codec_slice,
	codec_frame,
	codec_audio_service_type,
	codec_ma,
	codec_ef,
	codec_vi,
	codec_hi,
	codec_di,
	codec_co,
	codec_em,
	codec_vo,
	codec_ka,
	codec_request_sample_fmt,
	codec_pkt_timebase,
	codec_sub_charenc,
	codec_sub_charenc_mode,
	codec_do_nothing,
	codec_pre_decoder,
	codec_refcounted_frames,
	codec_side_data_only_packets,
	codec_skip_alpha,
	codec_field_order,
	codec_progressive,
	codec_tt,
	codec_tb,
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

import Foreign.C.Types
import Data.Int

import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- AVCodecContext:
-- ===========================

-- | Option "b" for AVCodecContext of type av_opt_type_flags.
-- set bitrate (in bits/s)
-- default value is AVOptionFlags 200000.
codec_b :: OptionName AVCodecContext CInt
codec_b = OptionName "b"

-- | Option "ab" for AVCodecContext of type av_opt_type_flags.
-- set bitrate (in bits/s)
-- default value is AVOptionFlags 128000.
codec_ab :: OptionName AVCodecContext CInt
codec_ab = OptionName "ab"

-- | Option "bt" for AVCodecContext of type av_opt_type_flags.
-- Set video bitrate tolerance (in bits/s). In 1-pass mode, bitrate tolerance
-- specifies how far ratecontrol is willing to deviate from the target average
-- bitrate value. This is not related to minimum/maximum bitrate. Lowering
-- tolerance too much has an adverse effect on quality.
-- default value is AVOptionFlags 4000000.
codec_bt :: OptionName AVCodecContext CInt
codec_bt = OptionName "bt"

-- | Option "flags" for AVCodecContext of type av_opt_type_flags-flags.
-- default value is AVOptionFlags 0.
codec_flags :: OptionName AVCodecContext CInt
codec_flags = OptionName "flags"

-- | Option "unaligned" for AVCodecContext of type av_opt_type_flags-flags.
-- allow decoders to produce unaligned output
-- default value is AVOptionFlags 1.
codec_unaligned :: OptionName AVCodecContext CInt
codec_unaligned = OptionName "unaligned"

-- | Option "mv4" for AVCodecContext of type av_opt_type_flags-flags.
-- use four motion vectors per macroblock (MPEG-4)
-- default value is AVOptionFlags 4.
codec_mv4 :: OptionName AVCodecContext CInt
codec_mv4 = OptionName "mv4"

-- | Option "qpel" for AVCodecContext of type av_opt_type_flags-flags.
-- use 1/4-pel motion compensation
-- default value is AVOptionFlags 16.
codec_qpel :: OptionName AVCodecContext CInt
codec_qpel = OptionName "qpel"

-- | Option "loop" for AVCodecContext of type av_opt_type_flags-flags.
-- use loop filter
-- default value is AVOptionFlags 2048.
codec_loop :: OptionName AVCodecContext CInt
codec_loop = OptionName "loop"

-- | Option "qscale" for AVCodecContext of type av_opt_type_flags-flags.
-- use fixed qscale
-- default value is AVOptionFlags 2.
codec_qscale :: OptionName AVCodecContext CInt
codec_qscale = OptionName "qscale"

-- | Option "gmc" for AVCodecContext of type av_opt_type_flags-flags.
-- use gmc
-- default value is AVOptionFlags 32.
codec_gmc :: OptionName AVCodecContext CInt
codec_gmc = OptionName "gmc"

-- | Option "mv0" for AVCodecContext of type av_opt_type_flags-flags.
-- always try a mb with mv=<0,0>
-- default value is AVOptionFlags 64.
codec_mv0 :: OptionName AVCodecContext CInt
codec_mv0 = OptionName "mv0"

-- | Option "input_preserved" for AVCodecContext of type av_opt_type_flags-flags.
-- default value is AVOptionFlags 256.
codec_input_preserved :: OptionName AVCodecContext CInt
codec_input_preserved = OptionName "input_preserved"

-- | Option "pass1" for AVCodecContext of type av_opt_type_flags-flags.
-- use internal 2-pass ratecontrol in first  pass mode
-- default value is AVOptionFlags 512.
codec_pass1 :: OptionName AVCodecContext CInt
codec_pass1 = OptionName "pass1"

-- | Option "pass2" for AVCodecContext of type av_opt_type_flags-flags.
-- use internal 2-pass ratecontrol in second pass mode
-- default value is AVOptionFlags 1024.
codec_pass2 :: OptionName AVCodecContext CInt
codec_pass2 = OptionName "pass2"

-- | Option "gray" for AVCodecContext of type av_opt_type_flags-flags.
-- only decode/encode grayscale
-- default value is AVOptionFlags 8192.
codec_gray :: OptionName AVCodecContext CInt
codec_gray = OptionName "gray"

-- | Option "emu_edge" for AVCodecContext of type av_opt_type_flags-flags.
-- do not draw edges
-- default value is AVOptionFlags 16384.
codec_emu_edge :: OptionName AVCodecContext CInt
codec_emu_edge = OptionName "emu_edge"

-- | Option "psnr" for AVCodecContext of type av_opt_type_flags-flags.
-- error[?] variables will be set during encoding
-- default value is AVOptionFlags 32768.
codec_psnr :: OptionName AVCodecContext CInt
codec_psnr = OptionName "psnr"

-- | Option "truncated" for AVCodecContext of type av_opt_type_flags-flags.
-- default value is AVOptionFlags 65536.
codec_truncated :: OptionName AVCodecContext CInt
codec_truncated = OptionName "truncated"

-- | Option "naq" for AVCodecContext of type av_opt_type_flags-flags.
-- normalize adaptive quantization
-- default value is AVOptionFlags 131072.
codec_naq :: OptionName AVCodecContext CInt
codec_naq = OptionName "naq"

-- | Option "ildct" for AVCodecContext of type av_opt_type_flags-flags.
-- use interlaced DCT
-- default value is AVOptionFlags 262144.
codec_ildct :: OptionName AVCodecContext CInt
codec_ildct = OptionName "ildct"

-- | Option "low_delay" for AVCodecContext of type av_opt_type_flags-flags.
-- force low delay
-- default value is AVOptionFlags 524288.
codec_low_delay :: OptionName AVCodecContext CInt
codec_low_delay = OptionName "low_delay"

-- | Option "global_header" for AVCodecContext of type av_opt_type_flags-flags.
-- place global headers in extradata instead of every keyframe
-- default value is AVOptionFlags 4194304.
codec_global_header :: OptionName AVCodecContext CInt
codec_global_header = OptionName "global_header"

-- | Option "bitexact" for AVCodecContext of type av_opt_type_flags-flags.
-- use only bitexact functions (except (I)DCT)
-- default value is AVOptionFlags 8388608.
codec_bitexact :: OptionName AVCodecContext CInt
codec_bitexact = OptionName "bitexact"

-- | Option "aic" for AVCodecContext of type av_opt_type_flags-flags.
-- H.263 advanced intra coding / MPEG-4 AC prediction
-- default value is AVOptionFlags 16777216.
codec_aic :: OptionName AVCodecContext CInt
codec_aic = OptionName "aic"

-- | Option "ilme" for AVCodecContext of type av_opt_type_flags-flags.
-- interlaced motion estimation
-- default value is AVOptionFlags 536870912.
codec_ilme :: OptionName AVCodecContext CInt
codec_ilme = OptionName "ilme"

-- | Option "cgop" for AVCodecContext of type av_opt_type_flags-flags.
-- closed GOP
-- default value is AVOptionFlags (-2147483648).
codec_cgop :: OptionName AVCodecContext CInt
codec_cgop = OptionName "cgop"

-- | Option "output_corrupt" for AVCodecContext of type av_opt_type_flags-flags.
-- Output even potentially corrupted frames
-- default value is AVOptionFlags 8.
codec_output_corrupt :: OptionName AVCodecContext CInt
codec_output_corrupt = OptionName "output_corrupt"

-- | Option "fast" for AVCodecContext of type av_opt_type_flags-flags2.
-- allow non-spec-compliant speedup tricks
-- default value is AVOptionFlags 1.
codec_fast :: OptionName AVCodecContext CInt
codec_fast = OptionName "fast"

-- | Option "noout" for AVCodecContext of type av_opt_type_flags-flags2.
-- skip bitstream encoding
-- default value is AVOptionFlags 4.
codec_noout :: OptionName AVCodecContext CInt
codec_noout = OptionName "noout"

-- | Option "ignorecrop" for AVCodecContext of type av_opt_type_flags-flags2.
-- ignore cropping information from sps
-- default value is AVOptionFlags 65536.
codec_ignorecrop :: OptionName AVCodecContext CInt
codec_ignorecrop = OptionName "ignorecrop"

-- | Option "local_header" for AVCodecContext of type av_opt_type_flags-flags2.
-- place global headers at every keyframe instead of in extradata
-- default value is AVOptionFlags 8.
codec_local_header :: OptionName AVCodecContext CInt
codec_local_header = OptionName "local_header"

-- | Option "chunks" for AVCodecContext of type av_opt_type_flags-flags2.
-- Frame data might be split into multiple chunks
-- default value is AVOptionFlags 32768.
codec_chunks :: OptionName AVCodecContext CInt
codec_chunks = OptionName "chunks"

-- | Option "showall" for AVCodecContext of type av_opt_type_flags-flags2.
-- Show all frames before the first keyframe
-- default value is AVOptionFlags 4194304.
codec_showall :: OptionName AVCodecContext CInt
codec_showall = OptionName "showall"

-- | Option "export_mvs" for AVCodecContext of type av_opt_type_flags-flags2.
-- export motion vectors through frame side data
-- default value is AVOptionFlags 268435456.
codec_export_mvs :: OptionName AVCodecContext CInt
codec_export_mvs = OptionName "export_mvs"

-- | Option "skip_manual" for AVCodecContext of type av_opt_type_flags-flags2.
-- do not skip samples and export skip information as frame side data
-- default value is AVOptionFlags 536870912.
codec_skip_manual :: OptionName AVCodecContext CInt
codec_skip_manual = OptionName "skip_manual"

-- | Option "me_method" for AVCodecContext of type av_opt_type_flags-me_method.
-- set motion estimation method
-- default value is AVOptionFlags 5.
codec_me_method :: OptionName AVCodecContext CInt
codec_me_method = OptionName "me_method"

-- | Option "zero" for AVCodecContext of type av_opt_type_flags-me_method.
-- zero motion estimation (fastest)
-- default value is AVOptionFlags 1.
codec_zero :: OptionName AVCodecContext CInt
codec_zero = OptionName "zero"

-- | Option "full" for AVCodecContext of type av_opt_type_flags-me_method.
-- full motion estimation (slowest)
-- default value is AVOptionFlags 2.
codec_full :: OptionName AVCodecContext CInt
codec_full = OptionName "full"

-- | Option "epzs" for AVCodecContext of type av_opt_type_flags-me_method.
-- EPZS motion estimation (default)
-- default value is AVOptionFlags 5.
codec_epzs :: OptionName AVCodecContext CInt
codec_epzs = OptionName "epzs"

-- | Option "esa" for AVCodecContext of type av_opt_type_flags-me_method.
-- esa motion estimation (alias for full)
-- default value is AVOptionFlags 2.
codec_esa :: OptionName AVCodecContext CInt
codec_esa = OptionName "esa"

-- | Option "tesa" for AVCodecContext of type av_opt_type_flags-me_method.
-- tesa motion estimation
-- default value is AVOptionFlags 9.
codec_tesa :: OptionName AVCodecContext CInt
codec_tesa = OptionName "tesa"

-- | Option "dia" for AVCodecContext of type av_opt_type_flags-me_method.
-- diamond motion estimation (alias for EPZS)
-- default value is AVOptionFlags 5.
codec_dia :: OptionName AVCodecContext CInt
codec_dia = OptionName "dia"

-- | Option "log" for AVCodecContext of type av_opt_type_flags-me_method.
-- log motion estimation
-- default value is AVOptionFlags 3.
codec_log :: OptionName AVCodecContext CInt
codec_log = OptionName "log"

-- | Option "phods" for AVCodecContext of type av_opt_type_flags-me_method.
-- phods motion estimation
-- default value is AVOptionFlags 4.
codec_phods :: OptionName AVCodecContext CInt
codec_phods = OptionName "phods"

-- | Option "x1" for AVCodecContext of type av_opt_type_flags-me_method.
-- X1 motion estimation
-- default value is AVOptionFlags 6.
codec_x1 :: OptionName AVCodecContext CInt
codec_x1 = OptionName "x1"

-- | Option "hex" for AVCodecContext of type av_opt_type_flags-me_method.
-- hex motion estimation
-- default value is AVOptionFlags 7.
codec_hex :: OptionName AVCodecContext CInt
codec_hex = OptionName "hex"

-- | Option "umh" for AVCodecContext of type av_opt_type_flags-me_method.
-- umh motion estimation
-- default value is AVOptionFlags 8.
codec_umh :: OptionName AVCodecContext CInt
codec_umh = OptionName "umh"

-- | Option "iter" for AVCodecContext of type av_opt_type_flags-me_method.
-- iter motion estimation
-- default value is AVOptionFlags 50.
codec_iter :: OptionName AVCodecContext CInt
codec_iter = OptionName "iter"

-- | Option "time_base" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_time_base :: OptionName AVCodecContext CInt
codec_time_base = OptionName "time_base"

-- | Option "g" for AVCodecContext of type av_opt_type_flags.
-- set the group of picture (GOP) size
-- default value is AVOptionFlags 12.
codec_g :: OptionName AVCodecContext CInt
codec_g = OptionName "g"

-- | Option "ar" for AVCodecContext of type av_opt_type_flags.
-- set audio sampling rate (in Hz)
-- default value is AVOptionFlags 0.
codec_ar :: OptionName AVCodecContext CInt
codec_ar = OptionName "ar"

-- | Option "ac" for AVCodecContext of type av_opt_type_flags.
-- set number of audio channels
-- default value is AVOptionFlags 0.
codec_ac :: OptionName AVCodecContext CInt
codec_ac = OptionName "ac"

-- | Option "cutoff" for AVCodecContext of type av_opt_type_flags.
-- set cutoff bandwidth
-- default value is AVOptionFlags 0.
codec_cutoff :: OptionName AVCodecContext CInt
codec_cutoff = OptionName "cutoff"

-- | Option "frame_size" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_frame_size :: OptionName AVCodecContext CInt
codec_frame_size = OptionName "frame_size"

-- | Option "frame_number" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_frame_number :: OptionName AVCodecContext CInt
codec_frame_number = OptionName "frame_number"

-- | Option "delay" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_delay :: OptionName AVCodecContext CInt
codec_delay = OptionName "delay"

-- | Option "qcomp" for AVCodecContext of type av_opt_type_flags.
-- video quantizer scale compression (VBR). Constant of ratecontrol equation.
-- Recommended range for default rc_eq: 0.0-1.0
-- default value is AVOptionFlags 0.
codec_qcomp :: OptionName AVCodecContext CInt
codec_qcomp = OptionName "qcomp"

-- | Option "qblur" for AVCodecContext of type av_opt_type_flags.
-- video quantizer scale blur (VBR)
-- default value is AVOptionFlags 0.
codec_qblur :: OptionName AVCodecContext CInt
codec_qblur = OptionName "qblur"

-- | Option "qmin" for AVCodecContext of type av_opt_type_flags.
-- minimum video quantizer scale (VBR)
-- default value is AVOptionFlags 2.
codec_qmin :: OptionName AVCodecContext CInt
codec_qmin = OptionName "qmin"

-- | Option "qmax" for AVCodecContext of type av_opt_type_flags.
-- maximum video quantizer scale (VBR)
-- default value is AVOptionFlags 31.
codec_qmax :: OptionName AVCodecContext CInt
codec_qmax = OptionName "qmax"

-- | Option "qdiff" for AVCodecContext of type av_opt_type_flags.
-- maximum difference between the quantizer scales (VBR)
-- default value is AVOptionFlags 3.
codec_qdiff :: OptionName AVCodecContext CInt
codec_qdiff = OptionName "qdiff"

-- | Option "bf" for AVCodecContext of type av_opt_type_flags.
-- set maximum number of B frames between non-B-frames
-- default value is AVOptionFlags 0.
codec_bf :: OptionName AVCodecContext CInt
codec_bf = OptionName "bf"

-- | Option "b_qfactor" for AVCodecContext of type av_opt_type_flags.
-- QP factor between P- and B-frames
-- default value is AVOptionFlags 0.
codec_b_qfactor :: OptionName AVCodecContext CInt
codec_b_qfactor = OptionName "b_qfactor"

-- | Option "rc_strategy" for AVCodecContext of type av_opt_type_flags.
-- ratecontrol method
-- default value is AVOptionFlags 0.
codec_rc_strategy :: OptionName AVCodecContext CInt
codec_rc_strategy = OptionName "rc_strategy"

-- | Option "b_strategy" for AVCodecContext of type av_opt_type_flags.
-- strategy to choose between I/P/B-frames
-- default value is AVOptionFlags 0.
codec_b_strategy :: OptionName AVCodecContext CInt
codec_b_strategy = OptionName "b_strategy"

-- | Option "ps" for AVCodecContext of type av_opt_type_flags.
-- RTP payload size in bytes
-- default value is AVOptionFlags 0.
codec_ps :: OptionName AVCodecContext CInt
codec_ps = OptionName "ps"

-- | Option "mv_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_mv_bits :: OptionName AVCodecContext CInt
codec_mv_bits = OptionName "mv_bits"

-- | Option "header_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_header_bits :: OptionName AVCodecContext CInt
codec_header_bits = OptionName "header_bits"

-- | Option "i_tex_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_i_tex_bits :: OptionName AVCodecContext CInt
codec_i_tex_bits = OptionName "i_tex_bits"

-- | Option "p_tex_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_p_tex_bits :: OptionName AVCodecContext CInt
codec_p_tex_bits = OptionName "p_tex_bits"

-- | Option "i_count" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_i_count :: OptionName AVCodecContext CInt
codec_i_count = OptionName "i_count"

-- | Option "p_count" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_p_count :: OptionName AVCodecContext CInt
codec_p_count = OptionName "p_count"

-- | Option "skip_count" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_skip_count :: OptionName AVCodecContext CInt
codec_skip_count = OptionName "skip_count"

-- | Option "misc_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_misc_bits :: OptionName AVCodecContext CInt
codec_misc_bits = OptionName "misc_bits"

-- | Option "frame_bits" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_frame_bits :: OptionName AVCodecContext CInt
codec_frame_bits = OptionName "frame_bits"

-- | Option "codec_tag" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_codec_tag :: OptionName AVCodecContext CInt
codec_codec_tag = OptionName "codec_tag"

-- | Option "bug" for AVCodecContext of type av_opt_type_flags-bug.
-- work around not autodetected encoder bugs
-- default value is AVOptionFlags 1.
codec_bug :: OptionName AVCodecContext CInt
codec_bug = OptionName "bug"

-- | Option "autodetect" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 1.
codec_autodetect :: OptionName AVCodecContext CInt
codec_autodetect = OptionName "autodetect"

-- | Option "old_msmpeg4" for AVCodecContext of type av_opt_type_flags-bug.
-- some old lavc-generated MSMPEG4v3 files (no autodetection)
-- default value is AVOptionFlags 2.
codec_old_msmpeg4 :: OptionName AVCodecContext CInt
codec_old_msmpeg4 = OptionName "old_msmpeg4"

-- | Option "xvid_ilace" for AVCodecContext of type av_opt_type_flags-bug.
-- Xvid interlacing bug (autodetected if FOURCC == XVIX)
-- default value is AVOptionFlags 4.
codec_xvid_ilace :: OptionName AVCodecContext CInt
codec_xvid_ilace = OptionName "xvid_ilace"

-- | Option "ump4" for AVCodecContext of type av_opt_type_flags-bug.
-- (autodetected if FOURCC == UMP4)
-- default value is AVOptionFlags 8.
codec_ump4 :: OptionName AVCodecContext CInt
codec_ump4 = OptionName "ump4"

-- | Option "no_padding" for AVCodecContext of type av_opt_type_flags-bug.
-- padding bug (autodetected)
-- default value is AVOptionFlags 16.
codec_no_padding :: OptionName AVCodecContext CInt
codec_no_padding = OptionName "no_padding"

-- | Option "amv" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 32.
codec_amv :: OptionName AVCodecContext CInt
codec_amv = OptionName "amv"

-- | Option "ac_vlc" for AVCodecContext of type av_opt_type_flags-bug.
-- illegal VLC bug (autodetected per FOURCC)
-- default value is AVOptionFlags 0.
codec_ac_vlc :: OptionName AVCodecContext CInt
codec_ac_vlc = OptionName "ac_vlc"

-- | Option "qpel_chroma" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 64.
codec_qpel_chroma :: OptionName AVCodecContext CInt
codec_qpel_chroma = OptionName "qpel_chroma"

-- | Option "std_qpel" for AVCodecContext of type av_opt_type_flags-bug.
-- old standard qpel (autodetected per FOURCC/version)
-- default value is AVOptionFlags 128.
codec_std_qpel :: OptionName AVCodecContext CInt
codec_std_qpel = OptionName "std_qpel"

-- | Option "qpel_chroma2" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 256.
codec_qpel_chroma2 :: OptionName AVCodecContext CInt
codec_qpel_chroma2 = OptionName "qpel_chroma2"

-- | Option "direct_blocksize" for AVCodecContext of type av_opt_type_flags-bug.
-- direct-qpel-blocksize bug (autodetected per FOURCC/version)
-- default value is AVOptionFlags 512.
codec_direct_blocksize :: OptionName AVCodecContext CInt
codec_direct_blocksize = OptionName "direct_blocksize"

-- | Option "edge" for AVCodecContext of type av_opt_type_flags-bug.
-- edge padding bug (autodetected per FOURCC/version)
-- default value is AVOptionFlags 1024.
codec_edge :: OptionName AVCodecContext CInt
codec_edge = OptionName "edge"

-- | Option "hpel_chroma" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 2048.
codec_hpel_chroma :: OptionName AVCodecContext CInt
codec_hpel_chroma = OptionName "hpel_chroma"

-- | Option "dc_clip" for AVCodecContext of type av_opt_type_flags-bug.
-- default value is AVOptionFlags 4096.
codec_dc_clip :: OptionName AVCodecContext CInt
codec_dc_clip = OptionName "dc_clip"

-- | Option "ms" for AVCodecContext of type av_opt_type_flags-bug.
-- work around various bugs in Microsoft's broken decoders
-- default value is AVOptionFlags 8192.
codec_ms :: OptionName AVCodecContext CInt
codec_ms = OptionName "ms"

-- | Option "trunc" for AVCodecContext of type av_opt_type_flags-bug.
-- truncated frames
-- default value is AVOptionFlags 16384.
codec_trunc :: OptionName AVCodecContext CInt
codec_trunc = OptionName "trunc"

-- | Option "strict" for AVCodecContext of type av_opt_type_flags-strict.
-- how strictly to follow the standards
-- default value is AVOptionFlags 0.
codec_strict :: OptionName AVCodecContext CInt
codec_strict = OptionName "strict"

-- | Option "very" for AVCodecContext of type av_opt_type_flags-strict.
-- strictly conform to a older more strict version of the spec or reference
-- software
-- default value is AVOptionFlags 2.
codec_very :: OptionName AVCodecContext CInt
codec_very = OptionName "very"

-- | Option "normal" for AVCodecContext of type av_opt_type_flags-strict.
-- default value is AVOptionFlags 0.
codec_normal :: OptionName AVCodecContext CInt
codec_normal = OptionName "normal"

-- | Option "unofficial" for AVCodecContext of type av_opt_type_flags-strict.
-- allow unofficial extensions
-- default value is AVOptionFlags (-1).
codec_unofficial :: OptionName AVCodecContext CInt
codec_unofficial = OptionName "unofficial"

-- | Option "experimental" for AVCodecContext of type av_opt_type_flags-strict.
-- allow non-standardized experimental things
-- default value is AVOptionFlags (-2).
codec_experimental :: OptionName AVCodecContext CInt
codec_experimental = OptionName "experimental"

-- | Option "b_qoffset" for AVCodecContext of type av_opt_type_flags.
-- QP offset between P- and B-frames
-- default value is AVOptionFlags 0.
codec_b_qoffset :: OptionName AVCodecContext CInt
codec_b_qoffset = OptionName "b_qoffset"

-- | Option "err_detect" for AVCodecContext of type av_opt_type_flags-err_detect.
-- set error detection flags
-- default value is AVOptionFlags 0.
codec_err_detect :: OptionName AVCodecContext CInt
codec_err_detect = OptionName "err_detect"

-- | Option "crccheck" for AVCodecContext of type av_opt_type_flags-err_detect.
-- verify embedded CRCs
-- default value is AVOptionFlags 1.
codec_crccheck :: OptionName AVCodecContext CInt
codec_crccheck = OptionName "crccheck"

-- | Option "bitstream" for AVCodecContext of type av_opt_type_flags-err_detect.
-- detect bitstream specification deviations
-- default value is AVOptionFlags 2.
codec_bitstream :: OptionName AVCodecContext CInt
codec_bitstream = OptionName "bitstream"

-- | Option "buffer" for AVCodecContext of type av_opt_type_flags-err_detect.
-- detect improper bitstream length
-- default value is AVOptionFlags 4.
codec_buffer :: OptionName AVCodecContext CInt
codec_buffer = OptionName "buffer"

-- | Option "explode" for AVCodecContext of type av_opt_type_flags-err_detect.
-- abort decoding on minor error detection
-- default value is AVOptionFlags 8.
codec_explode :: OptionName AVCodecContext CInt
codec_explode = OptionName "explode"

-- | Option "ignore_err" for AVCodecContext of type av_opt_type_flags-err_detect.
-- ignore errors
-- default value is AVOptionFlags 32768.
codec_ignore_err :: OptionName AVCodecContext CInt
codec_ignore_err = OptionName "ignore_err"

-- | Option "careful" for AVCodecContext of type av_opt_type_flags-err_detect.
-- consider things that violate the spec, are fast to check and have not been
-- seen in the wild as errors
-- default value is AVOptionFlags 65536.
codec_careful :: OptionName AVCodecContext CInt
codec_careful = OptionName "careful"

-- | Option "compliant" for AVCodecContext of type av_opt_type_flags-err_detect.
-- consider all spec non compliancies as errors
-- default value is AVOptionFlags 131072.
codec_compliant :: OptionName AVCodecContext CInt
codec_compliant = OptionName "compliant"

-- | Option "aggressive" for AVCodecContext of type av_opt_type_flags-err_detect.
-- consider things that a sane encoder should not do as an error
-- default value is AVOptionFlags 262144.
codec_aggressive :: OptionName AVCodecContext CInt
codec_aggressive = OptionName "aggressive"

-- | Option "has_b_frames" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_has_b_frames :: OptionName AVCodecContext CInt
codec_has_b_frames = OptionName "has_b_frames"

-- | Option "block_align" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_block_align :: OptionName AVCodecContext CInt
codec_block_align = OptionName "block_align"

-- | Option "mpeg_quant" for AVCodecContext of type av_opt_type_flags.
-- use MPEG quantizers instead of H.263
-- default value is AVOptionFlags 0.
codec_mpeg_quant :: OptionName AVCodecContext CInt
codec_mpeg_quant = OptionName "mpeg_quant"

-- | Option "qsquish" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_qsquish :: OptionName AVCodecContext CInt
codec_qsquish = OptionName "qsquish"

-- | Option "rc_qmod_amp" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_rc_qmod_amp :: OptionName AVCodecContext CInt
codec_rc_qmod_amp = OptionName "rc_qmod_amp"

-- | Option "rc_qmod_freq" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_rc_qmod_freq :: OptionName AVCodecContext CInt
codec_rc_qmod_freq = OptionName "rc_qmod_freq"

-- | Option "rc_override_count" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_rc_override_count :: OptionName AVCodecContext CInt
codec_rc_override_count = OptionName "rc_override_count"

-- | Option "rc_eq" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_rc_eq :: OptionName AVCodecContext CInt
codec_rc_eq = OptionName "rc_eq"

-- | Option "maxrate" for AVCodecContext of type av_opt_type_flags.
-- maximum bitrate (in bits/s). Used for VBV together with bufsize.
-- default value is AVOptionFlags 0.
codec_maxrate :: OptionName AVCodecContext CInt
codec_maxrate = OptionName "maxrate"

-- | Option "minrate" for AVCodecContext of type av_opt_type_flags.
-- minimum bitrate (in bits/s). Most useful in setting up a CBR encode. It is
-- of little use otherwise.
-- default value is AVOptionFlags 0.
codec_minrate :: OptionName AVCodecContext CInt
codec_minrate = OptionName "minrate"

-- | Option "bufsize" for AVCodecContext of type av_opt_type_flags.
-- set ratecontrol buffer size (in bits)
-- default value is AVOptionFlags 0.
codec_bufsize :: OptionName AVCodecContext CInt
codec_bufsize = OptionName "bufsize"

-- | Option "rc_buf_aggressivity" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_rc_buf_aggressivity :: OptionName AVCodecContext CInt
codec_rc_buf_aggressivity = OptionName "rc_buf_aggressivity"

-- | Option "i_qfactor" for AVCodecContext of type av_opt_type_flags.
-- QP factor between P- and I-frames
-- default value is AVOptionFlags (-1717986918).
codec_i_qfactor :: OptionName AVCodecContext CInt
codec_i_qfactor = OptionName "i_qfactor"

-- | Option "i_qoffset" for AVCodecContext of type av_opt_type_flags.
-- QP offset between P- and I-frames
-- default value is AVOptionFlags 0.
codec_i_qoffset :: OptionName AVCodecContext CInt
codec_i_qoffset = OptionName "i_qoffset"

-- | Option "rc_init_cplx" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_rc_init_cplx :: OptionName AVCodecContext CInt
codec_rc_init_cplx = OptionName "rc_init_cplx"

-- | Option "dct" for AVCodecContext of type av_opt_type_flags-dct.
-- DCT algorithm
-- default value is AVOptionFlags 0.
codec_dct :: OptionName AVCodecContext CInt
codec_dct = OptionName "dct"

-- | Option "auto" for AVCodecContext of type av_opt_type_flags-dct.
-- autoselect a good one (default)
-- default value is AVOptionFlags 0.
codec_auto :: OptionName AVCodecContext CInt
codec_auto = OptionName "auto"

-- | Option "fastint" for AVCodecContext of type av_opt_type_flags-dct.
-- fast integer
-- default value is AVOptionFlags 1.
codec_fastint :: OptionName AVCodecContext CInt
codec_fastint = OptionName "fastint"

-- | Option "int" for AVCodecContext of type av_opt_type_flags-dct.
-- accurate integer
-- default value is AVOptionFlags 2.
codec_int :: OptionName AVCodecContext CInt
codec_int = OptionName "int"

-- | Option "mmx" for AVCodecContext of type av_opt_type_flags-dct.
-- default value is AVOptionFlags 3.
codec_mmx :: OptionName AVCodecContext CInt
codec_mmx = OptionName "mmx"

-- | Option "altivec" for AVCodecContext of type av_opt_type_flags-dct.
-- default value is AVOptionFlags 5.
codec_altivec :: OptionName AVCodecContext CInt
codec_altivec = OptionName "altivec"

-- | Option "faan" for AVCodecContext of type av_opt_type_flags-dct.
-- floating point AAN DCT
-- default value is AVOptionFlags 6.
codec_faan :: OptionName AVCodecContext CInt
codec_faan = OptionName "faan"

-- | Option "lumi_mask" for AVCodecContext of type av_opt_type_flags.
-- compresses bright areas stronger than medium ones
-- default value is AVOptionFlags 0.
codec_lumi_mask :: OptionName AVCodecContext CInt
codec_lumi_mask = OptionName "lumi_mask"

-- | Option "tcplx_mask" for AVCodecContext of type av_opt_type_flags.
-- temporal complexity masking
-- default value is AVOptionFlags 0.
codec_tcplx_mask :: OptionName AVCodecContext CInt
codec_tcplx_mask = OptionName "tcplx_mask"

-- | Option "scplx_mask" for AVCodecContext of type av_opt_type_flags.
-- spatial complexity masking
-- default value is AVOptionFlags 0.
codec_scplx_mask :: OptionName AVCodecContext CInt
codec_scplx_mask = OptionName "scplx_mask"

-- | Option "p_mask" for AVCodecContext of type av_opt_type_flags.
-- inter masking
-- default value is AVOptionFlags 0.
codec_p_mask :: OptionName AVCodecContext CInt
codec_p_mask = OptionName "p_mask"

-- | Option "dark_mask" for AVCodecContext of type av_opt_type_flags.
-- compresses dark areas stronger than medium ones
-- default value is AVOptionFlags 0.
codec_dark_mask :: OptionName AVCodecContext CInt
codec_dark_mask = OptionName "dark_mask"

-- | Option "idct" for AVCodecContext of type av_opt_type_flags-idct.
-- select IDCT implementation
-- default value is AVOptionFlags 0.
codec_idct :: OptionName AVCodecContext CInt
codec_idct = OptionName "idct"

-- | Option "simple" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 2.
codec_simple :: OptionName AVCodecContext CInt
codec_simple = OptionName "simple"

-- | Option "simplemmx" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 3.
codec_simplemmx :: OptionName AVCodecContext CInt
codec_simplemmx = OptionName "simplemmx"

-- | Option "arm" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 7.
codec_arm :: OptionName AVCodecContext CInt
codec_arm = OptionName "arm"

-- | Option "sh4" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 9.
codec_sh4 :: OptionName AVCodecContext CInt
codec_sh4 = OptionName "sh4"

-- | Option "simplearm" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 10.
codec_simplearm :: OptionName AVCodecContext CInt
codec_simplearm = OptionName "simplearm"

-- | Option "simplearmv5te" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 16.
codec_simplearmv5te :: OptionName AVCodecContext CInt
codec_simplearmv5te = OptionName "simplearmv5te"

-- | Option "simplearmv6" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 17.
codec_simplearmv6 :: OptionName AVCodecContext CInt
codec_simplearmv6 = OptionName "simplearmv6"

-- | Option "simpleneon" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 22.
codec_simpleneon :: OptionName AVCodecContext CInt
codec_simpleneon = OptionName "simpleneon"

-- | Option "simplealpha" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 23.
codec_simplealpha :: OptionName AVCodecContext CInt
codec_simplealpha = OptionName "simplealpha"

-- | Option "ipp" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 13.
codec_ipp :: OptionName AVCodecContext CInt
codec_ipp = OptionName "ipp"

-- | Option "xvid" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 14.
codec_xvid :: OptionName AVCodecContext CInt
codec_xvid = OptionName "xvid"

-- | Option "xvidmmx" for AVCodecContext of type av_opt_type_flags-idct.
-- deprecated, for compatibility only
-- default value is AVOptionFlags 14.
codec_xvidmmx :: OptionName AVCodecContext CInt
codec_xvidmmx = OptionName "xvidmmx"

-- | Option "faani" for AVCodecContext of type av_opt_type_flags-idct.
-- floating point AAN IDCT
-- default value is AVOptionFlags 20.
codec_faani :: OptionName AVCodecContext CInt
codec_faani = OptionName "faani"

-- | Option "simpleauto" for AVCodecContext of type av_opt_type_flags-idct.
-- default value is AVOptionFlags 128.
codec_simpleauto :: OptionName AVCodecContext CInt
codec_simpleauto = OptionName "simpleauto"

-- | Option "slice_count" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_slice_count :: OptionName AVCodecContext CInt
codec_slice_count = OptionName "slice_count"

-- | Option "ec" for AVCodecContext of type av_opt_type_flags-ec.
-- set error concealment strategy
-- default value is AVOptionFlags 3.
codec_ec :: OptionName AVCodecContext CInt
codec_ec = OptionName "ec"

-- | Option "guess_mvs" for AVCodecContext of type av_opt_type_flags-ec.
-- iterative motion vector (MV) search (slow)
-- default value is AVOptionFlags 1.
codec_guess_mvs :: OptionName AVCodecContext CInt
codec_guess_mvs = OptionName "guess_mvs"

-- | Option "deblock" for AVCodecContext of type av_opt_type_flags-ec.
-- use strong deblock filter for damaged MBs
-- default value is AVOptionFlags 2.
codec_deblock :: OptionName AVCodecContext CInt
codec_deblock = OptionName "deblock"

-- | Option "favor_inter" for AVCodecContext of type av_opt_type_flags-ec.
-- favor predicting from the previous frame
-- default value is AVOptionFlags 256.
codec_favor_inter :: OptionName AVCodecContext CInt
codec_favor_inter = OptionName "favor_inter"

-- | Option "bits_per_coded_sample" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_bits_per_coded_sample :: OptionName AVCodecContext CInt
codec_bits_per_coded_sample = OptionName "bits_per_coded_sample"

-- | Option "pred" for AVCodecContext of type av_opt_type_flags-pred.
-- prediction method
-- default value is AVOptionFlags 0.
codec_pred :: OptionName AVCodecContext CInt
codec_pred = OptionName "pred"

-- | Option "left" for AVCodecContext of type av_opt_type_flags-pred.
-- default value is AVOptionFlags 0.
codec_left :: OptionName AVCodecContext CInt
codec_left = OptionName "left"

-- | Option "plane" for AVCodecContext of type av_opt_type_flags-pred.
-- default value is AVOptionFlags 1.
codec_plane :: OptionName AVCodecContext CInt
codec_plane = OptionName "plane"

-- | Option "median" for AVCodecContext of type av_opt_type_flags-pred.
-- default value is AVOptionFlags 2.
codec_median :: OptionName AVCodecContext CInt
codec_median = OptionName "median"

-- | Option "aspect" for AVCodecContext of type av_opt_type_flags.
-- sample aspect ratio
-- default value is AVOptionFlags 0.
codec_aspect :: OptionName AVCodecContext CInt
codec_aspect = OptionName "aspect"

-- | Option "debug" for AVCodecContext of type av_opt_type_flags-debug.
-- print specific debug info
-- default value is AVOptionFlags 0.
codec_debug :: OptionName AVCodecContext CInt
codec_debug = OptionName "debug"

-- | Option "pict" for AVCodecContext of type av_opt_type_flags-debug.
-- picture info
-- default value is AVOptionFlags 1.
codec_pict :: OptionName AVCodecContext CInt
codec_pict = OptionName "pict"

-- | Option "rc" for AVCodecContext of type av_opt_type_flags-debug.
-- rate control
-- default value is AVOptionFlags 2.
codec_rc :: OptionName AVCodecContext CInt
codec_rc = OptionName "rc"

-- | Option "mb_type" for AVCodecContext of type av_opt_type_flags-debug.
-- macroblock (MB) type
-- default value is AVOptionFlags 8.
codec_mb_type :: OptionName AVCodecContext CInt
codec_mb_type = OptionName "mb_type"

-- | Option "qp" for AVCodecContext of type av_opt_type_flags-debug.
-- per-block quantization parameter (QP)
-- default value is AVOptionFlags 16.
codec_qp :: OptionName AVCodecContext CInt
codec_qp = OptionName "qp"

-- | Option "mv" for AVCodecContext of type av_opt_type_flags-debug.
-- motion vector
-- default value is AVOptionFlags 32.
codec_mv :: OptionName AVCodecContext CInt
codec_mv = OptionName "mv"

-- | Option "dct_coeff" for AVCodecContext of type av_opt_type_flags-debug.
-- default value is AVOptionFlags 64.
codec_dct_coeff :: OptionName AVCodecContext CInt
codec_dct_coeff = OptionName "dct_coeff"

-- | Option "skip" for AVCodecContext of type av_opt_type_flags-debug.
-- default value is AVOptionFlags 128.
codec_skip :: OptionName AVCodecContext CInt
codec_skip = OptionName "skip"

-- | Option "startcode" for AVCodecContext of type av_opt_type_flags-debug.
-- default value is AVOptionFlags 256.
codec_startcode :: OptionName AVCodecContext CInt
codec_startcode = OptionName "startcode"

-- | Option "pts" for AVCodecContext of type av_opt_type_flags-debug.
-- default value is AVOptionFlags 512.
codec_pts :: OptionName AVCodecContext CInt
codec_pts = OptionName "pts"

-- | Option "er" for AVCodecContext of type av_opt_type_flags-debug.
-- error recognition
-- default value is AVOptionFlags 1024.
codec_er :: OptionName AVCodecContext CInt
codec_er = OptionName "er"

-- | Option "mmco" for AVCodecContext of type av_opt_type_flags-debug.
-- memory management control operations (H.264)
-- default value is AVOptionFlags 2048.
codec_mmco :: OptionName AVCodecContext CInt
codec_mmco = OptionName "mmco"

-- | Option "bugs" for AVCodecContext of type av_opt_type_flags-debug.
-- default value is AVOptionFlags 4096.
codec_bugs :: OptionName AVCodecContext CInt
codec_bugs = OptionName "bugs"

-- | Option "vis_qp" for AVCodecContext of type av_opt_type_flags-debug.
-- visualize quantization parameter (QP), lower QP are tinted greener
-- default value is AVOptionFlags 8192.
codec_vis_qp :: OptionName AVCodecContext CInt
codec_vis_qp = OptionName "vis_qp"

-- | Option "vis_mb_type" for AVCodecContext of type av_opt_type_flags-debug.
-- visualize block types
-- default value is AVOptionFlags 16384.
codec_vis_mb_type :: OptionName AVCodecContext CInt
codec_vis_mb_type = OptionName "vis_mb_type"

-- | Option "buffers" for AVCodecContext of type av_opt_type_flags-debug.
-- picture buffer allocations
-- default value is AVOptionFlags 32768.
codec_buffers :: OptionName AVCodecContext CInt
codec_buffers = OptionName "buffers"

-- | Option "thread_ops" for AVCodecContext of type av_opt_type_flags-debug.
-- threading operations
-- default value is AVOptionFlags 65536.
codec_thread_ops :: OptionName AVCodecContext CInt
codec_thread_ops = OptionName "thread_ops"

-- | Option "nomc" for AVCodecContext of type av_opt_type_flags-debug.
-- skip motion compensation
-- default value is AVOptionFlags 16777216.
codec_nomc :: OptionName AVCodecContext CInt
codec_nomc = OptionName "nomc"

-- | Option "vismv" for AVCodecContext of type av_opt_type_flags-debug_mv.
-- visualize motion vectors (MVs) (deprecated)
-- default value is AVOptionFlags 0.
codec_vismv :: OptionName AVCodecContext CInt
codec_vismv = OptionName "vismv"

-- | Option "pf" for AVCodecContext of type av_opt_type_flags-debug_mv.
-- forward predicted MVs of P-frames
-- default value is AVOptionFlags 1.
codec_pf :: OptionName AVCodecContext CInt
codec_pf = OptionName "pf"

-- | Option "bb" for AVCodecContext of type av_opt_type_flags-debug_mv.
-- backward predicted MVs of B-frames
-- default value is AVOptionFlags 4.
codec_bb :: OptionName AVCodecContext CInt
codec_bb = OptionName "bb"

-- | Option "cmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- full-pel ME compare function
-- default value is AVOptionFlags 0.
codec_cmp :: OptionName AVCodecContext CInt
codec_cmp = OptionName "cmp"

-- | Option "subcmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sub-pel ME compare function
-- default value is AVOptionFlags 0.
codec_subcmp :: OptionName AVCodecContext CInt
codec_subcmp = OptionName "subcmp"

-- | Option "mbcmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- macroblock compare function
-- default value is AVOptionFlags 0.
codec_mbcmp :: OptionName AVCodecContext CInt
codec_mbcmp = OptionName "mbcmp"

-- | Option "ildctcmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- interlaced DCT compare function
-- default value is AVOptionFlags 8.
codec_ildctcmp :: OptionName AVCodecContext CInt
codec_ildctcmp = OptionName "ildctcmp"

-- | Option "dia_size" for AVCodecContext of type av_opt_type_flags.
-- diamond type & size for motion estimation
-- default value is AVOptionFlags 0.
codec_dia_size :: OptionName AVCodecContext CInt
codec_dia_size = OptionName "dia_size"

-- | Option "last_pred" for AVCodecContext of type av_opt_type_flags.
-- amount of motion predictors from the previous frame
-- default value is AVOptionFlags 0.
codec_last_pred :: OptionName AVCodecContext CInt
codec_last_pred = OptionName "last_pred"

-- | Option "preme" for AVCodecContext of type av_opt_type_flags.
-- pre motion estimation
-- default value is AVOptionFlags 0.
codec_preme :: OptionName AVCodecContext CInt
codec_preme = OptionName "preme"

-- | Option "precmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- pre motion estimation compare function
-- default value is AVOptionFlags 0.
codec_precmp :: OptionName AVCodecContext CInt
codec_precmp = OptionName "precmp"

-- | Option "sad" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sum of absolute differences, fast (default)
-- default value is AVOptionFlags 0.
codec_sad :: OptionName AVCodecContext CInt
codec_sad = OptionName "sad"

-- | Option "sse" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sum of squared errors
-- default value is AVOptionFlags 1.
codec_sse :: OptionName AVCodecContext CInt
codec_sse = OptionName "sse"

-- | Option "satd" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sum of absolute Hadamard transformed differences
-- default value is AVOptionFlags 2.
codec_satd :: OptionName AVCodecContext CInt
codec_satd = OptionName "satd"

-- | Option "bit" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- number of bits needed for the block
-- default value is AVOptionFlags 5.
codec_bit :: OptionName AVCodecContext CInt
codec_bit = OptionName "bit"

-- | Option "rd" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- rate distortion optimal, slow
-- default value is AVOptionFlags 6.
codec_rd :: OptionName AVCodecContext CInt
codec_rd = OptionName "rd"

-- | Option "vsad" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sum of absolute vertical differences
-- default value is AVOptionFlags 8.
codec_vsad :: OptionName AVCodecContext CInt
codec_vsad = OptionName "vsad"

-- | Option "vsse" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- sum of squared vertical differences
-- default value is AVOptionFlags 9.
codec_vsse :: OptionName AVCodecContext CInt
codec_vsse = OptionName "vsse"

-- | Option "nsse" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- noise preserving sum of squared differences
-- default value is AVOptionFlags 10.
codec_nsse :: OptionName AVCodecContext CInt
codec_nsse = OptionName "nsse"

-- | Option "w53" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- 5/3 wavelet, only used in snow
-- default value is AVOptionFlags 11.
codec_w53 :: OptionName AVCodecContext CInt
codec_w53 = OptionName "w53"

-- | Option "w97" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- 9/7 wavelet, only used in snow
-- default value is AVOptionFlags 12.
codec_w97 :: OptionName AVCodecContext CInt
codec_w97 = OptionName "w97"

-- | Option "dctmax" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- default value is AVOptionFlags 13.
codec_dctmax :: OptionName AVCodecContext CInt
codec_dctmax = OptionName "dctmax"

-- | Option "chroma" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- default value is AVOptionFlags 256.
codec_chroma :: OptionName AVCodecContext CInt
codec_chroma = OptionName "chroma"

-- | Option "pre_dia_size" for AVCodecContext of type av_opt_type_flags.
-- diamond type & size for motion estimation pre-pass
-- default value is AVOptionFlags 0.
codec_pre_dia_size :: OptionName AVCodecContext CInt
codec_pre_dia_size = OptionName "pre_dia_size"

-- | Option "subq" for AVCodecContext of type av_opt_type_flags.
-- sub-pel motion estimation quality
-- default value is AVOptionFlags 8.
codec_subq :: OptionName AVCodecContext CInt
codec_subq = OptionName "subq"

-- | Option "dtg_active_format" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_dtg_active_format :: OptionName AVCodecContext CInt
codec_dtg_active_format = OptionName "dtg_active_format"

-- | Option "me_range" for AVCodecContext of type av_opt_type_flags.
-- limit motion vectors range (1023 for DivX player)
-- default value is AVOptionFlags 0.
codec_me_range :: OptionName AVCodecContext CInt
codec_me_range = OptionName "me_range"

-- | Option "ibias" for AVCodecContext of type av_opt_type_flags.
-- intra quant bias
-- default value is AVOptionFlags 999999.
codec_ibias :: OptionName AVCodecContext CInt
codec_ibias = OptionName "ibias"

-- | Option "pbias" for AVCodecContext of type av_opt_type_flags.
-- inter quant bias
-- default value is AVOptionFlags 999999.
codec_pbias :: OptionName AVCodecContext CInt
codec_pbias = OptionName "pbias"

-- | Option "global_quality" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_global_quality :: OptionName AVCodecContext CInt
codec_global_quality = OptionName "global_quality"

-- | Option "coder" for AVCodecContext of type av_opt_type_flags-coder.
-- default value is AVOptionFlags 0.
codec_coder :: OptionName AVCodecContext CInt
codec_coder = OptionName "coder"

-- | Option "vlc" for AVCodecContext of type av_opt_type_flags-coder.
-- variable length coder / Huffman coder
-- default value is AVOptionFlags 0.
codec_vlc :: OptionName AVCodecContext CInt
codec_vlc = OptionName "vlc"

-- | Option "raw" for AVCodecContext of type av_opt_type_flags-coder.
-- raw (no encoding)
-- default value is AVOptionFlags 2.
codec_raw :: OptionName AVCodecContext CInt
codec_raw = OptionName "raw"

-- | Option "rle" for AVCodecContext of type av_opt_type_flags-coder.
-- run-length coder
-- default value is AVOptionFlags 3.
codec_rle :: OptionName AVCodecContext CInt
codec_rle = OptionName "rle"

-- | Option "deflate" for AVCodecContext of type av_opt_type_flags-coder.
-- deflate-based coder
-- default value is AVOptionFlags 4.
codec_deflate :: OptionName AVCodecContext CInt
codec_deflate = OptionName "deflate"

-- | Option "context" for AVCodecContext of type av_opt_type_flags.
-- context model
-- default value is AVOptionFlags 0.
codec_context :: OptionName AVCodecContext CInt
codec_context = OptionName "context"

-- | Option "slice_flags" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_slice_flags :: OptionName AVCodecContext CInt
codec_slice_flags = OptionName "slice_flags"

-- | Option "xvmc_acceleration" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_xvmc_acceleration :: OptionName AVCodecContext CInt
codec_xvmc_acceleration = OptionName "xvmc_acceleration"

-- | Option "mbd" for AVCodecContext of type av_opt_type_flags-mbd.
-- macroblock decision algorithm (high quality mode)
-- default value is AVOptionFlags 0.
codec_mbd :: OptionName AVCodecContext CInt
codec_mbd = OptionName "mbd"

-- | Option "bits" for AVCodecContext of type av_opt_type_flags-mbd.
-- use fewest bits
-- default value is AVOptionFlags 1.
codec_bits :: OptionName AVCodecContext CInt
codec_bits = OptionName "bits"

-- | Option "stream_codec_tag" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_stream_codec_tag :: OptionName AVCodecContext CInt
codec_stream_codec_tag = OptionName "stream_codec_tag"

-- | Option "sc_threshold" for AVCodecContext of type av_opt_type_flags.
-- scene change threshold
-- default value is AVOptionFlags 0.
codec_sc_threshold :: OptionName AVCodecContext CInt
codec_sc_threshold = OptionName "sc_threshold"

-- | Option "lmin" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_lmin :: OptionName AVCodecContext CInt
codec_lmin = OptionName "lmin"

-- | Option "lmax" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_lmax :: OptionName AVCodecContext CInt
codec_lmax = OptionName "lmax"

-- | Option "nr" for AVCodecContext of type av_opt_type_flags.
-- noise reduction
-- default value is AVOptionFlags 0.
codec_nr :: OptionName AVCodecContext CInt
codec_nr = OptionName "nr"

-- | Option "rc_init_occupancy" for AVCodecContext of type av_opt_type_flags.
-- number of bits which should be loaded into the rc buffer before decoding
-- starts
-- default value is AVOptionFlags 0.
codec_rc_init_occupancy :: OptionName AVCodecContext CInt
codec_rc_init_occupancy = OptionName "rc_init_occupancy"

-- | Option "flags2" for AVCodecContext of type av_opt_type_flags-flags2.
-- default value is AVOptionFlags 0.
codec_flags2 :: OptionName AVCodecContext CInt
codec_flags2 = OptionName "flags2"

-- | Option "error" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_error :: OptionName AVCodecContext CInt
codec_error = OptionName "error"

-- | Option "threads" for AVCodecContext of type av_opt_type_flags-threads.
-- default value is AVOptionFlags 1.
codec_threads :: OptionName AVCodecContext CInt
codec_threads = OptionName "threads"

-- | Option "me_threshold" for AVCodecContext of type av_opt_type_flags.
-- motion estimation threshold
-- default value is AVOptionFlags 0.
codec_me_threshold :: OptionName AVCodecContext CInt
codec_me_threshold = OptionName "me_threshold"

-- | Option "mb_threshold" for AVCodecContext of type av_opt_type_flags.
-- macroblock threshold
-- default value is AVOptionFlags 0.
codec_mb_threshold :: OptionName AVCodecContext CInt
codec_mb_threshold = OptionName "mb_threshold"

-- | Option "dc" for AVCodecContext of type av_opt_type_flags.
-- intra_dc_precision
-- default value is AVOptionFlags 0.
codec_dc :: OptionName AVCodecContext CInt
codec_dc = OptionName "dc"

-- | Option "nssew" for AVCodecContext of type av_opt_type_flags.
-- nsse weight
-- default value is AVOptionFlags 8.
codec_nssew :: OptionName AVCodecContext CInt
codec_nssew = OptionName "nssew"

-- | Option "skip_top" for AVCodecContext of type av_opt_type_flags.
-- number of macroblock rows at the top which are skipped
-- default value is AVOptionFlags 0.
codec_skip_top :: OptionName AVCodecContext CInt
codec_skip_top = OptionName "skip_top"

-- | Option "skip_bottom" for AVCodecContext of type av_opt_type_flags.
-- number of macroblock rows at the bottom which are skipped
-- default value is AVOptionFlags 0.
codec_skip_bottom :: OptionName AVCodecContext CInt
codec_skip_bottom = OptionName "skip_bottom"

-- | Option "profile" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags (-99).
codec_profile :: OptionName AVCodecContext CInt
codec_profile = OptionName "profile"

-- | Option "unknown" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags (-99).
codec_unknown :: OptionName AVCodecContext CInt
codec_unknown = OptionName "unknown"

-- | Option "aac_main" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 0.
codec_aac_main :: OptionName AVCodecContext CInt
codec_aac_main = OptionName "aac_main"

-- | Option "aac_low" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 1.
codec_aac_low :: OptionName AVCodecContext CInt
codec_aac_low = OptionName "aac_low"

-- | Option "aac_ssr" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 2.
codec_aac_ssr :: OptionName AVCodecContext CInt
codec_aac_ssr = OptionName "aac_ssr"

-- | Option "aac_ltp" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 3.
codec_aac_ltp :: OptionName AVCodecContext CInt
codec_aac_ltp = OptionName "aac_ltp"

-- | Option "aac_he" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 4.
codec_aac_he :: OptionName AVCodecContext CInt
codec_aac_he = OptionName "aac_he"

-- | Option "aac_he_v2" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 28.
codec_aac_he_v2 :: OptionName AVCodecContext CInt
codec_aac_he_v2 = OptionName "aac_he_v2"

-- | Option "aac_ld" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 22.
codec_aac_ld :: OptionName AVCodecContext CInt
codec_aac_ld = OptionName "aac_ld"

-- | Option "aac_eld" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 38.
codec_aac_eld :: OptionName AVCodecContext CInt
codec_aac_eld = OptionName "aac_eld"

-- | Option "mpeg2_aac_low" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 128.
codec_mpeg2_aac_low :: OptionName AVCodecContext CInt
codec_mpeg2_aac_low = OptionName "mpeg2_aac_low"

-- | Option "mpeg2_aac_he" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 131.
codec_mpeg2_aac_he :: OptionName AVCodecContext CInt
codec_mpeg2_aac_he = OptionName "mpeg2_aac_he"

-- | Option "dts" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 20.
codec_dts :: OptionName AVCodecContext CInt
codec_dts = OptionName "dts"

-- | Option "dts_es" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 30.
codec_dts_es :: OptionName AVCodecContext CInt
codec_dts_es = OptionName "dts_es"

-- | Option "dts_96_24" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 40.
codec_dts_96_24 :: OptionName AVCodecContext CInt
codec_dts_96_24 = OptionName "dts_96_24"

-- | Option "dts_hd_hra" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 50.
codec_dts_hd_hra :: OptionName AVCodecContext CInt
codec_dts_hd_hra = OptionName "dts_hd_hra"

-- | Option "dts_hd_ma" for AVCodecContext of type av_opt_type_flags-profile.
-- default value is AVOptionFlags 60.
codec_dts_hd_ma :: OptionName AVCodecContext CInt
codec_dts_hd_ma = OptionName "dts_hd_ma"

-- | Option "level" for AVCodecContext of type av_opt_type_flags-level.
-- default value is AVOptionFlags (-99).
codec_level :: OptionName AVCodecContext CInt
codec_level = OptionName "level"

-- | Option "lowres" for AVCodecContext of type av_opt_type_flags.
-- decode at 1= 1/2, 2=1/4, 3=1/8 resolutions
-- default value is AVOptionFlags 0.
codec_lowres :: OptionName AVCodecContext CInt
codec_lowres = OptionName "lowres"

-- | Option "skip_threshold" for AVCodecContext of type av_opt_type_flags.
-- frame skip threshold
-- default value is AVOptionFlags 0.
codec_skip_threshold :: OptionName AVCodecContext CInt
codec_skip_threshold = OptionName "skip_threshold"

-- | Option "skip_factor" for AVCodecContext of type av_opt_type_flags.
-- frame skip factor
-- default value is AVOptionFlags 0.
codec_skip_factor :: OptionName AVCodecContext CInt
codec_skip_factor = OptionName "skip_factor"

-- | Option "skip_exp" for AVCodecContext of type av_opt_type_flags.
-- frame skip exponent
-- default value is AVOptionFlags 0.
codec_skip_exp :: OptionName AVCodecContext CInt
codec_skip_exp = OptionName "skip_exp"

-- | Option "skipcmp" for AVCodecContext of type av_opt_type_flags-cmp_func.
-- frame skip compare function
-- default value is AVOptionFlags 13.
codec_skipcmp :: OptionName AVCodecContext CInt
codec_skipcmp = OptionName "skipcmp"

-- | Option "border_mask" for AVCodecContext of type av_opt_type_flags.
-- deprecated, use encoder private options instead
-- default value is AVOptionFlags 0.
codec_border_mask :: OptionName AVCodecContext CInt
codec_border_mask = OptionName "border_mask"

-- | Option "mblmin" for AVCodecContext of type av_opt_type_flags.
-- minimum macroblock Lagrange factor (VBR)
-- default value is AVOptionFlags 236.
codec_mblmin :: OptionName AVCodecContext CInt
codec_mblmin = OptionName "mblmin"

-- | Option "mblmax" for AVCodecContext of type av_opt_type_flags.
-- maximum macroblock Lagrange factor (VBR)
-- default value is AVOptionFlags 3658.
codec_mblmax :: OptionName AVCodecContext CInt
codec_mblmax = OptionName "mblmax"

-- | Option "mepc" for AVCodecContext of type av_opt_type_flags.
-- motion estimation bitrate penalty compensation (1.0 = 256)
-- default value is AVOptionFlags 256.
codec_mepc :: OptionName AVCodecContext CInt
codec_mepc = OptionName "mepc"

-- | Option "skip_loop_filter" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- skip loop filtering process for the selected frames
-- default value is AVOptionFlags 0.
codec_skip_loop_filter :: OptionName AVCodecContext CInt
codec_skip_loop_filter = OptionName "skip_loop_filter"

-- | Option "skip_idct" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- skip IDCT/dequantization for the selected frames
-- default value is AVOptionFlags 0.
codec_skip_idct :: OptionName AVCodecContext CInt
codec_skip_idct = OptionName "skip_idct"

-- | Option "skip_frame" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- skip decoding for the selected frames
-- default value is AVOptionFlags 0.
codec_skip_frame :: OptionName AVCodecContext CInt
codec_skip_frame = OptionName "skip_frame"

-- | Option "none" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard no frame
-- default value is AVOptionFlags (-16).
codec_none :: OptionName AVCodecContext CInt
codec_none = OptionName "none"

-- | Option "default" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard useless frames
-- default value is AVOptionFlags 0.
codec_default :: OptionName AVCodecContext CInt
codec_default = OptionName "default"

-- | Option "noref" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard all non-reference frames
-- default value is AVOptionFlags 8.
codec_noref :: OptionName AVCodecContext CInt
codec_noref = OptionName "noref"

-- | Option "bidir" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard all bidirectional frames
-- default value is AVOptionFlags 16.
codec_bidir :: OptionName AVCodecContext CInt
codec_bidir = OptionName "bidir"

-- | Option "nokey" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard all frames except keyframes
-- default value is AVOptionFlags 32.
codec_nokey :: OptionName AVCodecContext CInt
codec_nokey = OptionName "nokey"

-- | Option "nointra" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard all frames except I frames
-- default value is AVOptionFlags 24.
codec_nointra :: OptionName AVCodecContext CInt
codec_nointra = OptionName "nointra"

-- | Option "all" for AVCodecContext of type av_opt_type_flags-avdiscard.
-- discard all frames
-- default value is AVOptionFlags 48.
codec_all :: OptionName AVCodecContext CInt
codec_all = OptionName "all"

-- | Option "bidir_refine" for AVCodecContext of type av_opt_type_flags.
-- refine the two motion vectors used in bidirectional macroblocks
-- default value is AVOptionFlags 1.
codec_bidir_refine :: OptionName AVCodecContext CInt
codec_bidir_refine = OptionName "bidir_refine"

-- | Option "brd_scale" for AVCodecContext of type av_opt_type_flags.
-- downscale frames for dynamic B-frame decision
-- default value is AVOptionFlags 0.
codec_brd_scale :: OptionName AVCodecContext CInt
codec_brd_scale = OptionName "brd_scale"

-- | Option "keyint_min" for AVCodecContext of type av_opt_type_flags.
-- minimum interval between IDR-frames
-- default value is AVOptionFlags 25.
codec_keyint_min :: OptionName AVCodecContext CInt
codec_keyint_min = OptionName "keyint_min"

-- | Option "refs" for AVCodecContext of type av_opt_type_flags.
-- reference frames to consider for motion compensation
-- default value is AVOptionFlags 1.
codec_refs :: OptionName AVCodecContext CInt
codec_refs = OptionName "refs"

-- | Option "chromaoffset" for AVCodecContext of type av_opt_type_flags.
-- chroma QP offset from luma
-- default value is AVOptionFlags 0.
codec_chromaoffset :: OptionName AVCodecContext CInt
codec_chromaoffset = OptionName "chromaoffset"

-- | Option "trellis" for AVCodecContext of type av_opt_type_flags.
-- rate-distortion optimal quantization
-- default value is AVOptionFlags 0.
codec_trellis :: OptionName AVCodecContext CInt
codec_trellis = OptionName "trellis"

-- | Option "sc_factor" for AVCodecContext of type av_opt_type_flags.
-- multiplied by qscale for each frame and added to scene_change_score
-- default value is AVOptionFlags 6.
codec_sc_factor :: OptionName AVCodecContext CInt
codec_sc_factor = OptionName "sc_factor"

-- | Option "mv0_threshold" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 256.
codec_mv0_threshold :: OptionName AVCodecContext CInt
codec_mv0_threshold = OptionName "mv0_threshold"

-- | Option "b_sensitivity" for AVCodecContext of type av_opt_type_flags.
-- adjust sensitivity of b_frame_strategy 1
-- default value is AVOptionFlags 40.
codec_b_sensitivity :: OptionName AVCodecContext CInt
codec_b_sensitivity = OptionName "b_sensitivity"

-- | Option "compression_level" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
codec_compression_level :: OptionName AVCodecContext CInt
codec_compression_level = OptionName "compression_level"

-- | Option "min_prediction_order" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
codec_min_prediction_order :: OptionName AVCodecContext CInt
codec_min_prediction_order = OptionName "min_prediction_order"

-- | Option "max_prediction_order" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
codec_max_prediction_order :: OptionName AVCodecContext CInt
codec_max_prediction_order = OptionName "max_prediction_order"

-- | Option "timecode_frame_start" for AVCodecContext of type av_opt_type_flags.
-- GOP timecode frame start number, in non-drop-frame format
-- default value is AVOptionFlags (-1).
codec_timecode_frame_start :: OptionName AVCodecContext CInt
codec_timecode_frame_start = OptionName "timecode_frame_start"

-- | Option "request_channels" for AVCodecContext of type av_opt_type_flags.
-- set desired number of audio channels
-- default value is AVOptionFlags 0.
codec_request_channels :: OptionName AVCodecContext CInt
codec_request_channels = OptionName "request_channels"

-- | Option "bits_per_raw_sample" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_bits_per_raw_sample :: OptionName AVCodecContext CInt
codec_bits_per_raw_sample = OptionName "bits_per_raw_sample"

-- | Option "channel_layout" for AVCodecContext of type av_opt_type_flags-channel_layout.
-- default value is AVOptionFlags 0.
codec_channel_layout :: OptionName AVCodecContext CInt
codec_channel_layout = OptionName "channel_layout"

-- | Option "request_channel_layout" for AVCodecContext of type av_opt_type_flags-request_channel_layout.
-- default value is AVOptionFlags 0.
codec_request_channel_layout :: OptionName AVCodecContext CInt
codec_request_channel_layout = OptionName "request_channel_layout"

-- | Option "rc_max_vbv_use" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_rc_max_vbv_use :: OptionName AVCodecContext CInt
codec_rc_max_vbv_use = OptionName "rc_max_vbv_use"

-- | Option "rc_min_vbv_use" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_rc_min_vbv_use :: OptionName AVCodecContext CInt
codec_rc_min_vbv_use = OptionName "rc_min_vbv_use"

-- | Option "ticks_per_frame" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 1.
codec_ticks_per_frame :: OptionName AVCodecContext CInt
codec_ticks_per_frame = OptionName "ticks_per_frame"

-- | Option "color_primaries" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- color primaries
-- default value is AVOptionFlags 2.
codec_color_primaries :: OptionName AVCodecContext CInt
codec_color_primaries = OptionName "color_primaries"

-- | Option "bt709" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- BT.709
-- default value is AVOptionFlags 1.
codec_bt709 :: OptionName AVCodecContext CInt
codec_bt709 = OptionName "bt709"

-- | Option "unspecified" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- Unspecified
-- default value is AVOptionFlags 2.
codec_unspecified :: OptionName AVCodecContext CInt
codec_unspecified = OptionName "unspecified"

-- | Option "bt470m" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- BT.470 M
-- default value is AVOptionFlags 4.
codec_bt470m :: OptionName AVCodecContext CInt
codec_bt470m = OptionName "bt470m"

-- | Option "bt470bg" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- BT.470 BG
-- default value is AVOptionFlags 5.
codec_bt470bg :: OptionName AVCodecContext CInt
codec_bt470bg = OptionName "bt470bg"

-- | Option "smpte170m" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- SMPTE 170 M
-- default value is AVOptionFlags 6.
codec_smpte170m :: OptionName AVCodecContext CInt
codec_smpte170m = OptionName "smpte170m"

-- | Option "smpte240m" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- SMPTE 240 M
-- default value is AVOptionFlags 7.
codec_smpte240m :: OptionName AVCodecContext CInt
codec_smpte240m = OptionName "smpte240m"

-- | Option "film" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- Film
-- default value is AVOptionFlags 8.
codec_film :: OptionName AVCodecContext CInt
codec_film = OptionName "film"

-- | Option "bt2020" for AVCodecContext of type av_opt_type_flags-color_primaries_type.
-- BT.2020
-- default value is AVOptionFlags 9.
codec_bt2020 :: OptionName AVCodecContext CInt
codec_bt2020 = OptionName "bt2020"

-- | Option "color_trc" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- color transfer characteristics
-- default value is AVOptionFlags 2.
codec_color_trc :: OptionName AVCodecContext CInt
codec_color_trc = OptionName "color_trc"

-- | Option "gamma22" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- BT.470 M
-- default value is AVOptionFlags 4.
codec_gamma22 :: OptionName AVCodecContext CInt
codec_gamma22 = OptionName "gamma22"

-- | Option "gamma28" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- BT.470 BG
-- default value is AVOptionFlags 5.
codec_gamma28 :: OptionName AVCodecContext CInt
codec_gamma28 = OptionName "gamma28"

-- | Option "linear" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- Linear
-- default value is AVOptionFlags 8.
codec_linear :: OptionName AVCodecContext CInt
codec_linear = OptionName "linear"

-- | Option "log_sqrt" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- Log square root
-- default value is AVOptionFlags 10.
codec_log_sqrt :: OptionName AVCodecContext CInt
codec_log_sqrt = OptionName "log_sqrt"

-- | Option "iec61966_2_4" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- IEC 61966-2-4
-- default value is AVOptionFlags 11.
codec_iec61966_2_4 :: OptionName AVCodecContext CInt
codec_iec61966_2_4 = OptionName "iec61966_2_4"

-- | Option "bt1361" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- BT.1361
-- default value is AVOptionFlags 12.
codec_bt1361 :: OptionName AVCodecContext CInt
codec_bt1361 = OptionName "bt1361"

-- | Option "iec61966_2_1" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- IEC 61966-2-1
-- default value is AVOptionFlags 13.
codec_iec61966_2_1 :: OptionName AVCodecContext CInt
codec_iec61966_2_1 = OptionName "iec61966_2_1"

-- | Option "bt2020_10bit" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- BT.2020 - 10 bit
-- default value is AVOptionFlags 14.
codec_bt2020_10bit :: OptionName AVCodecContext CInt
codec_bt2020_10bit = OptionName "bt2020_10bit"

-- | Option "bt2020_12bit" for AVCodecContext of type av_opt_type_flags-color_trc_type.
-- BT.2020 - 12 bit
-- default value is AVOptionFlags 15.
codec_bt2020_12bit :: OptionName AVCodecContext CInt
codec_bt2020_12bit = OptionName "bt2020_12bit"

-- | Option "colorspace" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- color space
-- default value is AVOptionFlags 2.
codec_colorspace :: OptionName AVCodecContext CInt
codec_colorspace = OptionName "colorspace"

-- | Option "rgb" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- RGB
-- default value is AVOptionFlags 0.
codec_rgb :: OptionName AVCodecContext CInt
codec_rgb = OptionName "rgb"

-- | Option "fcc" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- FCC
-- default value is AVOptionFlags 4.
codec_fcc :: OptionName AVCodecContext CInt
codec_fcc = OptionName "fcc"

-- | Option "ycocg" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- YCOCG
-- default value is AVOptionFlags 8.
codec_ycocg :: OptionName AVCodecContext CInt
codec_ycocg = OptionName "ycocg"

-- | Option "bt2020_ncl" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- BT.2020 NCL
-- default value is AVOptionFlags 9.
codec_bt2020_ncl :: OptionName AVCodecContext CInt
codec_bt2020_ncl = OptionName "bt2020_ncl"

-- | Option "bt2020_cl" for AVCodecContext of type av_opt_type_flags-colorspace_type.
-- BT.2020 CL
-- default value is AVOptionFlags 10.
codec_bt2020_cl :: OptionName AVCodecContext CInt
codec_bt2020_cl = OptionName "bt2020_cl"

-- | Option "color_range" for AVCodecContext of type av_opt_type_flags-color_range_type.
-- color range
-- default value is AVOptionFlags 0.
codec_color_range :: OptionName AVCodecContext CInt
codec_color_range = OptionName "color_range"

-- | Option "mpeg" for AVCodecContext of type av_opt_type_flags-color_range_type.
-- MPEG (219*2^(n-8))
-- default value is AVOptionFlags 1.
codec_mpeg :: OptionName AVCodecContext CInt
codec_mpeg = OptionName "mpeg"

-- | Option "jpeg" for AVCodecContext of type av_opt_type_flags-color_range_type.
-- JPEG (2^n-1)
-- default value is AVOptionFlags 2.
codec_jpeg :: OptionName AVCodecContext CInt
codec_jpeg = OptionName "jpeg"

-- | Option "chroma_sample_location" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- chroma sample location
-- default value is AVOptionFlags 0.
codec_chroma_sample_location :: OptionName AVCodecContext CInt
codec_chroma_sample_location = OptionName "chroma_sample_location"

-- | Option "center" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- Center
-- default value is AVOptionFlags 2.
codec_center :: OptionName AVCodecContext CInt
codec_center = OptionName "center"

-- | Option "topleft" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- Top-left
-- default value is AVOptionFlags 3.
codec_topleft :: OptionName AVCodecContext CInt
codec_topleft = OptionName "topleft"

-- | Option "top" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- Top
-- default value is AVOptionFlags 4.
codec_top :: OptionName AVCodecContext CInt
codec_top = OptionName "top"

-- | Option "bottomleft" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- Bottom-left
-- default value is AVOptionFlags 5.
codec_bottomleft :: OptionName AVCodecContext CInt
codec_bottomleft = OptionName "bottomleft"

-- | Option "bottom" for AVCodecContext of type av_opt_type_flags-chroma_sample_location_type.
-- Bottom
-- default value is AVOptionFlags 6.
codec_bottom :: OptionName AVCodecContext CInt
codec_bottom = OptionName "bottom"

-- | Option "log_level_offset" for AVCodecContext of type av_opt_type_flags.
-- set the log level offset
-- default value is AVOptionFlags 0.
codec_log_level_offset :: OptionName AVCodecContext CInt
codec_log_level_offset = OptionName "log_level_offset"

-- | Option "slices" for AVCodecContext of type av_opt_type_flags.
-- number of slices, used in parallelized encoding
-- default value is AVOptionFlags 0.
codec_slices :: OptionName AVCodecContext CInt
codec_slices = OptionName "slices"

-- | Option "thread_type" for AVCodecContext of type av_opt_type_flags-thread_type.
-- select multithreading type
-- default value is AVOptionFlags 3.
codec_thread_type :: OptionName AVCodecContext CInt
codec_thread_type = OptionName "thread_type"

-- | Option "slice" for AVCodecContext of type av_opt_type_flags-thread_type.
-- default value is AVOptionFlags 2.
codec_slice :: OptionName AVCodecContext CInt
codec_slice = OptionName "slice"

-- | Option "frame" for AVCodecContext of type av_opt_type_flags-thread_type.
-- default value is AVOptionFlags 1.
codec_frame :: OptionName AVCodecContext CInt
codec_frame = OptionName "frame"

-- | Option "audio_service_type" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- audio service type
-- default value is AVOptionFlags 0.
codec_audio_service_type :: OptionName AVCodecContext CInt
codec_audio_service_type = OptionName "audio_service_type"

-- | Option "ma" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Main Audio Service
-- default value is AVOptionFlags 0.
codec_ma :: OptionName AVCodecContext CInt
codec_ma = OptionName "ma"

-- | Option "ef" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Effects
-- default value is AVOptionFlags 1.
codec_ef :: OptionName AVCodecContext CInt
codec_ef = OptionName "ef"

-- | Option "vi" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Visually Impaired
-- default value is AVOptionFlags 2.
codec_vi :: OptionName AVCodecContext CInt
codec_vi = OptionName "vi"

-- | Option "hi" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Hearing Impaired
-- default value is AVOptionFlags 3.
codec_hi :: OptionName AVCodecContext CInt
codec_hi = OptionName "hi"

-- | Option "di" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Dialogue
-- default value is AVOptionFlags 4.
codec_di :: OptionName AVCodecContext CInt
codec_di = OptionName "di"

-- | Option "co" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Commentary
-- default value is AVOptionFlags 5.
codec_co :: OptionName AVCodecContext CInt
codec_co = OptionName "co"

-- | Option "em" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Emergency
-- default value is AVOptionFlags 6.
codec_em :: OptionName AVCodecContext CInt
codec_em = OptionName "em"

-- | Option "vo" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Voice Over
-- default value is AVOptionFlags 7.
codec_vo :: OptionName AVCodecContext CInt
codec_vo = OptionName "vo"

-- | Option "ka" for AVCodecContext of type av_opt_type_flags-audio_service_type.
-- Karaoke
-- default value is AVOptionFlags 8.
codec_ka :: OptionName AVCodecContext CInt
codec_ka = OptionName "ka"

-- | Option "request_sample_fmt" for AVCodecContext of type av_opt_type_flags-request_sample_fmt.
-- sample format audio decoders should prefer
-- default value is AVOptionFlags (-1).
codec_request_sample_fmt :: OptionName AVCodecContext CInt
codec_request_sample_fmt = OptionName "request_sample_fmt"

-- | Option "pkt_timebase" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_pkt_timebase :: OptionName AVCodecContext CInt
codec_pkt_timebase = OptionName "pkt_timebase"

-- | Option "sub_charenc" for AVCodecContext of type av_opt_type_flags.
-- set input text subtitles character encoding
-- default value is AVOptionFlags 0.
codec_sub_charenc :: OptionName AVCodecContext CInt
codec_sub_charenc = OptionName "sub_charenc"

-- | Option "sub_charenc_mode" for AVCodecContext of type av_opt_type_flags-sub_charenc_mode.
-- set input text subtitles character encoding mode
-- default value is AVOptionFlags 0.
codec_sub_charenc_mode :: OptionName AVCodecContext CInt
codec_sub_charenc_mode = OptionName "sub_charenc_mode"

-- | Option "do_nothing" for AVCodecContext of type av_opt_type_flags-sub_charenc_mode.
-- default value is AVOptionFlags (-1).
codec_do_nothing :: OptionName AVCodecContext CInt
codec_do_nothing = OptionName "do_nothing"

-- | Option "pre_decoder" for AVCodecContext of type av_opt_type_flags-sub_charenc_mode.
-- default value is AVOptionFlags 1.
codec_pre_decoder :: OptionName AVCodecContext CInt
codec_pre_decoder = OptionName "pre_decoder"

-- | Option "refcounted_frames" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_refcounted_frames :: OptionName AVCodecContext CInt
codec_refcounted_frames = OptionName "refcounted_frames"

-- | Option "side_data_only_packets" for AVCodecContext of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
codec_side_data_only_packets :: OptionName AVCodecContext CInt
codec_side_data_only_packets = OptionName "side_data_only_packets"

-- | Option "skip_alpha" for AVCodecContext of type av_opt_type_flags.
-- Skip processing alpha
-- default value is AVOptionFlags 0.
codec_skip_alpha :: OptionName AVCodecContext CInt
codec_skip_alpha = OptionName "skip_alpha"

-- | Option "field_order" for AVCodecContext of type av_opt_type_flags-field_order.
-- Field order
-- default value is AVOptionFlags 0.
codec_field_order :: OptionName AVCodecContext CInt
codec_field_order = OptionName "field_order"

-- | Option "progressive" for AVCodecContext of type av_opt_type_flags-field_order.
-- default value is AVOptionFlags 1.
codec_progressive :: OptionName AVCodecContext CInt
codec_progressive = OptionName "progressive"

-- | Option "tt" for AVCodecContext of type av_opt_type_flags-field_order.
-- default value is AVOptionFlags 2.
codec_tt :: OptionName AVCodecContext CInt
codec_tt = OptionName "tt"

-- | Option "tb" for AVCodecContext of type av_opt_type_flags-field_order.
-- default value is AVOptionFlags 4.
codec_tb :: OptionName AVCodecContext CInt
codec_tb = OptionName "tb"

-- | Option "dump_separator" for AVCodecContext of type av_opt_type_flags.
-- set information dump field separator
-- default value is AVOptionFlags 0.
codec_dump_separator :: OptionName AVCodecContext CInt
codec_dump_separator = OptionName "dump_separator"

-- | Option "codec_whitelist" for AVCodecContext of type av_opt_type_flags.
-- List of decoders that are allowed to be used
-- default value is AVOptionFlags 0.
codec_codec_whitelist :: OptionName AVCodecContext CInt
codec_codec_whitelist = OptionName "codec_whitelist"

-- | Option "pixel_format" for AVCodecContext of type av_opt_type_flags.
-- set pixel format
-- default value is AVOptionFlags (-1).
codec_pixel_format :: OptionName AVCodecContext CInt
codec_pixel_format = OptionName "pixel_format"

-- | Option "video_size" for AVCodecContext of type av_opt_type_flags.
-- set video size
-- default value is AVOptionFlags 0.
codec_video_size :: OptionName AVCodecContext CInt
codec_video_size = OptionName "video_size"

-- AVFrame:
-- ===========================

-- | Option "best_effort_timestamp" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_best_effort_timestamp :: OptionName AVFrame CInt
frame_best_effort_timestamp = OptionName "best_effort_timestamp"

-- | Option "pkt_pos" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
frame_pkt_pos :: OptionName AVFrame CInt
frame_pkt_pos = OptionName "pkt_pos"

-- | Option "pkt_size" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
frame_pkt_size :: OptionName AVFrame CInt
frame_pkt_size = OptionName "pkt_size"

-- | Option "sample_aspect_ratio" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_sample_aspect_ratio :: OptionName AVFrame CInt
frame_sample_aspect_ratio = OptionName "sample_aspect_ratio"

-- | Option "width" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_width :: OptionName AVFrame CInt
frame_width = OptionName "width"

-- | Option "height" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_height :: OptionName AVFrame CInt
frame_height = OptionName "height"

-- | Option "format" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags (-1).
frame_format :: OptionName AVFrame CInt
frame_format = OptionName "format"

-- | Option "channel_layout" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_channel_layout :: OptionName AVFrame CInt
frame_channel_layout = OptionName "channel_layout"

-- | Option "sample_rate" for AVFrame of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
frame_sample_rate :: OptionName AVFrame CInt
frame_sample_rate = OptionName "sample_rate"

-- AVSubtitleRect:
-- ===========================

-- | Option "x" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_x :: OptionName AVSubtitleRect CInt
subtitle_x = OptionName "x"

-- | Option "y" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_y :: OptionName AVSubtitleRect CInt
subtitle_y = OptionName "y"

-- | Option "w" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_w :: OptionName AVSubtitleRect CInt
subtitle_w = OptionName "w"

-- | Option "h" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_h :: OptionName AVSubtitleRect CInt
subtitle_h = OptionName "h"

-- | Option "type" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_type :: OptionName AVSubtitleRect CInt
subtitle_type = OptionName "type"

-- | Option "flags" for AVSubtitleRect of type av_opt_type_flags-flags.
-- default value is AVOptionFlags 0.
subtitle_flags :: OptionName AVSubtitleRect CInt
subtitle_flags = OptionName "flags"

-- | Option "forced" for AVSubtitleRect of type av_opt_type_flags.
-- default value is AVOptionFlags 0.
subtitle_forced :: OptionName AVSubtitleRect CInt
subtitle_forced = OptionName "forced"

