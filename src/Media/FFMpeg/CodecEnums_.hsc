---- -*- haskell -*- 
{-# LANGUAGE ForeignFunctionInterface #-}

{- | Module 'Media.FFMpeg.CodecEnums_' implements 
    enumerations from libavcodec
   
   (c) 2009 Vasyl Pasternak
 -}


module Media.FFMpeg.CodecEnums_
    (
     CodecType (..)
    ,SampleFormat (..)
    ,CodecId (..)
    ) where

#include "ffmpeg.h"
#include "macros.hsc2hs.h"

--
-- | CodecType
--
#{begin_enum CodecType, CODEC_TYPE_UNKNOWN}
#{add_enum CODEC_TYPE_VIDEO}
#{add_enum CODEC_TYPE_AUDIO}
#{add_enum CODEC_TYPE_DATA}
#{add_enum CODEC_TYPE_SUBTITLE}
-- #{add_enum CODEC_TYPE_ATTACHMENT}
#{add_enum CODEC_TYPE_NB}
#{end_enum "Eq,Ord,Show"}

-- 
-- | SampleFormat
--
#{begin_enum SampleFormat, SAMPLE_FMT_NONE}
#{add_enum SAMPLE_FMT_U8}
#{add_enum SAMPLE_FMT_S16}
#{add_enum SAMPLE_FMT_S32}
#{add_enum SAMPLE_FMT_FLT}
-- #{add_enum SAMPLE_FMT_DBL}
-- #{add_enum SAMPLE_FMT_NB}
#{end_enum "Eq,Ord,Show"}

--
-- | CodecId
--
#{begin_enum CodecId, CODEC_ID_NONE}
#{add_enum CODEC_ID_MPEG1VIDEO}
#{add_enum CODEC_ID_MPEG2VIDEO}
#{add_enum CODEC_ID_MPEG2VIDEO_XVMC}
#{add_enum CODEC_ID_H261}
#{add_enum CODEC_ID_H263}
#{add_enum CODEC_ID_RV10}
#{add_enum CODEC_ID_RV20}
#{add_enum CODEC_ID_MJPEG}
#{add_enum CODEC_ID_MJPEGB}
#{add_enum CODEC_ID_LJPEG}
#{add_enum CODEC_ID_SP5X}
#{add_enum CODEC_ID_JPEGLS}
#{add_enum CODEC_ID_MPEG4}
#{add_enum CODEC_ID_RAWVIDEO}
#{add_enum CODEC_ID_MSMPEG4V1}
#{add_enum CODEC_ID_MSMPEG4V2}
#{add_enum CODEC_ID_MSMPEG4V3}
#{add_enum CODEC_ID_WMV1}
#{add_enum CODEC_ID_WMV2}
#{add_enum CODEC_ID_H263P}
#{add_enum CODEC_ID_H263I}
#{add_enum CODEC_ID_FLV1}
#{add_enum CODEC_ID_SVQ1}
#{add_enum CODEC_ID_SVQ3}
#{add_enum CODEC_ID_DVVIDEO}
#{add_enum CODEC_ID_HUFFYUV}
#{add_enum CODEC_ID_CYUV}
#{add_enum CODEC_ID_H264}
#{add_enum CODEC_ID_INDEO3}
#{add_enum CODEC_ID_VP3}
#{add_enum CODEC_ID_THEORA}
#{add_enum CODEC_ID_ASV1}
#{add_enum CODEC_ID_ASV2}
#{add_enum CODEC_ID_FFV1}
#{add_enum CODEC_ID_4XM}
#{add_enum CODEC_ID_VCR1}
#{add_enum CODEC_ID_CLJR}
#{add_enum CODEC_ID_MDEC}
#{add_enum CODEC_ID_ROQ}
#{add_enum CODEC_ID_INTERPLAY_VIDEO}
#{add_enum CODEC_ID_XAN_WC3}
#{add_enum CODEC_ID_XAN_WC4}
#{add_enum CODEC_ID_RPZA}
#{add_enum CODEC_ID_CINEPAK}
#{add_enum CODEC_ID_WS_VQA}
#{add_enum CODEC_ID_MSRLE}
#{add_enum CODEC_ID_MSVIDEO1}
#{add_enum CODEC_ID_IDCIN}
#{add_enum CODEC_ID_8BPS}
#{add_enum CODEC_ID_SMC}
#{add_enum CODEC_ID_FLIC}
#{add_enum CODEC_ID_TRUEMOTION1}
#{add_enum CODEC_ID_VMDVIDEO}
#{add_enum CODEC_ID_MSZH}
#{add_enum CODEC_ID_ZLIB}
#{add_enum CODEC_ID_QTRLE}
#{add_enum CODEC_ID_SNOW}
#{add_enum CODEC_ID_TSCC}
#{add_enum CODEC_ID_ULTI}
#{add_enum CODEC_ID_QDRAW}
#{add_enum CODEC_ID_VIXL}
#{add_enum CODEC_ID_QPEG}
#{add_enum CODEC_ID_XVID}
#{add_enum CODEC_ID_PNG}
#{add_enum CODEC_ID_PPM}
#{add_enum CODEC_ID_PBM}
#{add_enum CODEC_ID_PGM}
#{add_enum CODEC_ID_PGMYUV}
#{add_enum CODEC_ID_PAM}
#{add_enum CODEC_ID_FFVHUFF}
#{add_enum CODEC_ID_RV30}
#{add_enum CODEC_ID_RV40}
#{add_enum CODEC_ID_VC1}
#{add_enum CODEC_ID_WMV3}
#{add_enum CODEC_ID_LOCO}
#{add_enum CODEC_ID_WNV1}
#{add_enum CODEC_ID_AASC}
#{add_enum CODEC_ID_INDEO2}
#{add_enum CODEC_ID_FRAPS}
#{add_enum CODEC_ID_TRUEMOTION2}
#{add_enum CODEC_ID_BMP}
#{add_enum CODEC_ID_CSCD}
#{add_enum CODEC_ID_MMVIDEO}
#{add_enum CODEC_ID_ZMBV}
#{add_enum CODEC_ID_AVS}
#{add_enum CODEC_ID_SMACKVIDEO}
#{add_enum CODEC_ID_NUV}
#{add_enum CODEC_ID_KMVC}
#{add_enum CODEC_ID_FLASHSV}
#{add_enum CODEC_ID_CAVS}
#{add_enum CODEC_ID_JPEG2000}
#{add_enum CODEC_ID_VMNC}
#{add_enum CODEC_ID_VP5}
#{add_enum CODEC_ID_VP6}
#{add_enum CODEC_ID_VP6F}
#{add_enum CODEC_ID_TARGA}
#{add_enum CODEC_ID_DSICINVIDEO}
#{add_enum CODEC_ID_TIERTEXSEQVIDEO}
#{add_enum CODEC_ID_TIFF}
#{add_enum CODEC_ID_GIF}
#{add_enum CODEC_ID_FFH264}
-- #{add_enum CODEC_ID_DXA}
-- #{add_enum CODEC_ID_DNXHD}
-- #{add_enum CODEC_ID_THP}
-- #{add_enum CODEC_ID_SGI}
-- #{add_enum CODEC_ID_C93}
-- #{add_enum CODEC_ID_BETHSOFTVID}
-- #{add_enum CODEC_ID_PTX}
-- #{add_enum CODEC_ID_TXD}
-- #{add_enum CODEC_ID_VP6A}
-- #{add_enum CODEC_ID_AMV}
-- #{add_enum CODEC_ID_VB}
-- #{add_enum CODEC_ID_PCX}
-- #{add_enum CODEC_ID_SUNRAST}
-- #{add_enum CODEC_ID_INDEO4}
-- #{add_enum CODEC_ID_INDEO5}
-- #{add_enum CODEC_ID_MIMIC}
-- #{add_enum CODEC_ID_RL2}
-- #{add_enum CODEC_ID_8SVX_EXP}
-- #{add_enum CODEC_ID_8SVX_FIB}
-- #{add_enum CODEC_ID_ESCAPE124}
-- #{add_enum CODEC_ID_DIRAC}
-- #{add_enum CODEC_ID_BFI}
-- #{add_enum CODEC_ID_CMV}
-- #{add_enum CODEC_ID_MOTIONPIXELS}
-- #{add_enum CODEC_ID_TGV}
-- #{add_enum CODEC_ID_TGQ}
-- #{add_enum CODEC_ID_TQI}
-- #{add_enum CODEC_ID_AURA}
-- #{add_enum CODEC_ID_AURA2}
#{add_enum CODEC_ID_PCM_S16LE}
#{add_enum CODEC_ID_PCM_S16BE}
#{add_enum CODEC_ID_PCM_U16LE}
#{add_enum CODEC_ID_PCM_U16BE}
#{add_enum CODEC_ID_PCM_S8}
#{add_enum CODEC_ID_PCM_U8}
#{add_enum CODEC_ID_PCM_MULAW}
#{add_enum CODEC_ID_PCM_ALAW}
#{add_enum CODEC_ID_PCM_S32LE}
#{add_enum CODEC_ID_PCM_S32BE}
#{add_enum CODEC_ID_PCM_U32LE}
#{add_enum CODEC_ID_PCM_U32BE}
#{add_enum CODEC_ID_PCM_S24LE}
#{add_enum CODEC_ID_PCM_S24BE}
#{add_enum CODEC_ID_PCM_U24LE}
#{add_enum CODEC_ID_PCM_U24BE}
#{add_enum CODEC_ID_PCM_S24DAUD}
-- #{add_enum CODEC_ID_PCM_ZORK}
-- #{add_enum CODEC_ID_PCM_S16LE_PLANAR}
-- #{add_enum CODEC_ID_PCM_DVD}
-- #{add_enum CODEC_ID_PCM_F32BE}
-- #{add_enum CODEC_ID_PCM_F32LE}
-- #{add_enum CODEC_ID_PCM_F64BE}
-- #{add_enum CODEC_ID_PCM_F64LE}
#{add_enum CODEC_ID_ADPCM_IMA_QT}
#{add_enum CODEC_ID_ADPCM_IMA_WAV}
#{add_enum CODEC_ID_ADPCM_IMA_DK3}
#{add_enum CODEC_ID_ADPCM_IMA_DK4}
#{add_enum CODEC_ID_ADPCM_IMA_WS}
#{add_enum CODEC_ID_ADPCM_IMA_SMJPEG}
#{add_enum CODEC_ID_ADPCM_MS}
#{add_enum CODEC_ID_ADPCM_4XM}
#{add_enum CODEC_ID_ADPCM_XA}
#{add_enum CODEC_ID_ADPCM_ADX}
#{add_enum CODEC_ID_ADPCM_EA}
#{add_enum CODEC_ID_ADPCM_G726}
#{add_enum CODEC_ID_ADPCM_CT}
#{add_enum CODEC_ID_ADPCM_SWF}
#{add_enum CODEC_ID_ADPCM_YAMAHA}
#{add_enum CODEC_ID_ADPCM_SBPRO_4}
#{add_enum CODEC_ID_ADPCM_SBPRO_3}
#{add_enum CODEC_ID_ADPCM_SBPRO_2}
-- #{add_enum CODEC_ID_ADPCM_THP}
-- #{add_enum CODEC_ID_ADPCM_IMA_AMV}
-- #{add_enum CODEC_ID_ADPCM_EA_R1}
-- #{add_enum CODEC_ID_ADPCM_EA_R3}
-- #{add_enum CODEC_ID_ADPCM_EA_R2}
-- #{add_enum CODEC_ID_ADPCM_IMA_EA_SEAD}
-- #{add_enum CODEC_ID_ADPCM_IMA_EA_EACS}
-- #{add_enum CODEC_ID_ADPCM_EA_XAS}
-- #{add_enum CODEC_ID_ADPCM_EA_MAXIS_XA}
-- #{add_enum CODEC_ID_ADPCM_IMA_ISS}
#{add_enum CODEC_ID_AMR_NB}
#{add_enum CODEC_ID_AMR_WB}
#{add_enum CODEC_ID_RA_144}
#{add_enum CODEC_ID_RA_288}
#{add_enum CODEC_ID_ROQ_DPCM}
#{add_enum CODEC_ID_INTERPLAY_DPCM}
#{add_enum CODEC_ID_XAN_DPCM}
#{add_enum CODEC_ID_SOL_DPCM}
#{add_enum CODEC_ID_MP2}
#{add_enum CODEC_ID_MP3}
#{add_enum CODEC_ID_AAC}
#{add_enum CODEC_ID_AC3}
#{add_enum CODEC_ID_DTS}
#{add_enum CODEC_ID_VORBIS}
#{add_enum CODEC_ID_DVAUDIO}
#{add_enum CODEC_ID_WMAV1}
#{add_enum CODEC_ID_WMAV2}
#{add_enum CODEC_ID_MACE3}
#{add_enum CODEC_ID_MACE6}
#{add_enum CODEC_ID_VMDAUDIO}
#{add_enum CODEC_ID_SONIC}
#{add_enum CODEC_ID_SONIC_LS}
#{add_enum CODEC_ID_FLAC}
#{add_enum CODEC_ID_MP3ADU}
#{add_enum CODEC_ID_MP3ON4}
#{add_enum CODEC_ID_SHORTEN}
#{add_enum CODEC_ID_ALAC}
#{add_enum CODEC_ID_WESTWOOD_SND1}
#{add_enum CODEC_ID_GSM}
#{add_enum CODEC_ID_QDM2}
#{add_enum CODEC_ID_COOK}
#{add_enum CODEC_ID_TRUESPEECH}
#{add_enum CODEC_ID_TTA}
#{add_enum CODEC_ID_SMACKAUDIO}
#{add_enum CODEC_ID_QCELP}
#{add_enum CODEC_ID_WAVPACK}
#{add_enum CODEC_ID_DSICINAUDIO}
#{add_enum CODEC_ID_IMC}
#{add_enum CODEC_ID_MUSEPACK7}
#{add_enum CODEC_ID_MLP}
#{add_enum CODEC_ID_GSM_MS}
-- #{add_enum CODEC_ID_ATRAC3}
-- #{add_enum CODEC_ID_VOXWARE}
-- #{add_enum CODEC_ID_APE}
-- #{add_enum CODEC_ID_NELLYMOSER}
-- #{add_enum CODEC_ID_MUSEPACK8}
-- #{add_enum CODEC_ID_SPEEX}
-- #{add_enum CODEC_ID_WMAVOICE}
-- #{add_enum CODEC_ID_WMAPRO}
-- #{add_enum CODEC_ID_WMALOSSLESS}
-- #{add_enum CODEC_ID_ATRAC3P}
-- #{add_enum CODEC_ID_EAC3}
-- #{add_enum CODEC_ID_SIPR}
-- #{add_enum CODEC_ID_MP1}
-- #{add_enum CODEC_ID_TWINVQ}
-- #{add_enum CODEC_ID_TRUEHD}
#{add_enum CODEC_ID_DVD_SUBTITLE}
#{add_enum CODEC_ID_DVB_SUBTITLE}
-- #{add_enum CODEC_ID_TEXT}
-- #{add_enum CODEC_ID_XSUB}
-- #{add_enum CODEC_ID_SSA}
-- #{add_enum CODEC_ID_MOV_TEXT}
-- #{add_enum CODEC_ID_TTF}
-- #{add_enum CODEC_ID_PROBE}
#{add_enum CODEC_ID_MPEG2TS}
#{end_enum "Eq,Ord,Show"}



