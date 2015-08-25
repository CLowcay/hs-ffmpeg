{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

{- |

Description : Enumerations for libavcodec
Copyright   : (c) Vasyl Pasternak, 2009
                  Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Enumerations for libavcodec.

-}

module Media.FFMpeg.Codec.Enums (
	AVMediaType,
	pattern AVMediaTypeUnknown,
	pattern AVMediaTypeVideo,
	pattern AVMediaTypeAudio,
	pattern AVMediaTypeData,
	pattern AVMediaTypeSubtitle,
	pattern AVMediaTypeAttachment,

	AVSampleFormat,
	pattern AVSampleFmtNone,
	pattern AVSampleFmtU8,
	pattern AVSampleFmtS16,
	pattern AVSampleFmtS32,
	pattern AVSampleFmtFlt,
	pattern AVSampleFmtDbl,
	pattern AVSampleFmtU8p,
	pattern AVSampleFmtS16p,
	pattern AVSampleFmtS32p,
	pattern AVSampleFmtFltp,
	pattern AVSampleFmtDblp,

	AVPacketSideDataType,
	pattern AVPktDataPalette,
	pattern AVPktDataNewExtradata,
	pattern AVPktDataParamChange,
	pattern AVPktDataH263MbInfo,
	pattern AVPktDataReplayGain,
	pattern AVPktDataDisplayMatrix,
	pattern AVPktDataStereo3d,
	--pattern AVPktDataAudioServiceType,
	--pattern AVPktDataQualityFactor,
	pattern AVPktDataSkipSamples,
	pattern AVPktDataJpDualmono,
	pattern AVPktDataStringsMetadata,
	pattern AVPktDataSubtitlePosition,
	pattern AVPktDataMatroskaBlockadditional,
	pattern AVPktDataWebvttIdentifier,
	pattern AVPktDataWebvttSettings,
	pattern AVPktDataMetadataUpdate,

	AVPacketFlag,
	pattern AVPktFlagKey,
	pattern AVPktFlagCorrupt,

	AVSideDataParamChangeFlags,
	pattern AVSideDataParamChangeChannelCount,
	pattern AVSideDataParamChangeChannelLayout,
	pattern AVSideDataParamChangeSampleRate,
	pattern AVSideDataParamChangeDimensions,

	AVCodecProp,
	pattern AVCodecPropIntraOnly,
	pattern AVCodecPropLossy,
	pattern AVCodecPropLossless,
	pattern AVCodecPropReorder,
	pattern AVCodecPropBitmapSub,
	pattern AVCodecPropTextSub,

	pattern FFMaxBFrames,
	pattern FFCompressionDefault,

	AVCodecFlag,
	pattern CodecFlagUnaligned,
	pattern CodecFlagQscale,
	pattern CodecFlag4mv,
	pattern CodecFlagOutputCorrupt,
	pattern CodecFlagQpel,
	pattern CodecFlagGmc,
	pattern CodecFlagMv0,
	pattern CodecFlagInputPreserved,
	pattern CodecFlagPass1,
	pattern CodecFlagPass2,
	pattern CodecFlagGray,
	pattern CodecFlagEmuEdge,
	pattern CodecFlagPsnr,
	pattern CodecFlagTruncated,
	pattern CodecFlagNormalizeAqp,
	pattern CodecFlagInterlacedDct,
	pattern CodecFlagLowDelay,
	pattern CodecFlagGlobalHeader,
	pattern CodecFlagBitexact,
	pattern CodecFlagAcPred,
	pattern CodecFlagLoopFilter,
	pattern CodecFlagInterlacedMe,
	pattern CodecFlagClosedGop,

	AVCodecFlag2,
	pattern CodecFlag2Fast,
	pattern CodecFlag2NoOutput,
	pattern CodecFlag2LocalHeader,
	pattern CodecFlag2DropFrameTimecode,
	pattern CodecFlag2IgnoreCrop,
	pattern CodecFlag2Chunks,
	pattern CodecFlag2ShowAll,
	pattern CodecFlag2ExportMvs,
	pattern CodecFlag2SkipManual,

	AVCodecCap,
	pattern CodecCapDrawHorizBand,
	pattern CodecCapDr1,
	pattern CodecCapTruncated,
	pattern CodecCapDelay,
	pattern CodecCapSmallLastFrame,
	pattern CodecCapHwaccelVdpau,
	pattern CodecCapSubframes,
	pattern CodecCapExperimental,
	pattern CodecCapChannelConf,
	pattern CodecCapNegLinesizes,
	pattern CodecCapFrameThreads,
	pattern CodecCapSliceThreads,
	pattern CodecCapParamChange,
	pattern CodecCapAutoThreads,
	pattern CodecCapVariableFrameSize,
	pattern CodecCapIntraOnly,
	pattern CodecCapLossless,

	AVMBType,
	pattern MBTypeIntra4X4,
	pattern MBTypeIntra16X16,
	pattern MBTypeIntraPcm,
	pattern MBType16X16,
	pattern MBType16X8,
	pattern MBType8X16,
	pattern MBType8X8,
	pattern MBTypeInterlaced,
	pattern MBTypeDirect2,
	pattern MBTypeAcpred,
	pattern MBTypeGmc,
	pattern MBTypeSkip,
	pattern MBTypeP0l0,
	pattern MBTypeP1l0,
	pattern MBTypeP0l1,
	pattern MBTypeP1l1,
	pattern MBTypeL0,
	pattern MBTypeL1,
	pattern MBTypeL0l1,
	pattern MBTypeQuant,
	pattern MBTypeCbp,

	FFQscaleType,
	pattern FFQscaleTypeMpeg1,
	pattern FFQscaleTypeMpeg2,
	pattern FFQscaleTypeH264,
	pattern FFQscaleTypeVp56,

	FFBufferType,
	pattern FFBufferTypeInternal,
	pattern FFBufferTypeUser,
	pattern FFBufferTypeShared,
	pattern FFBufferTypeCopy,

	FFBufferHints,
	pattern FFBufferHintsValid,
	pattern FFBufferHintsReadable,
	pattern FFBufferHintsPreserve,
	pattern FFBufferHintsReusable,

	AVGetBufferFlag,
	pattern AVGetBufferFlagRef,

	AVSubtitleFlag,
	pattern AVSubtitleFlagForced,

	AVAudioServiceType,
	pattern AVAudioServiceTypeMain,
	pattern AVAudioServiceTypeEffects,
	pattern AVAudioServiceTypeVisuallyImpaired,
	pattern AVAudioServiceTypeHearingImpaired,
	pattern AVAudioServiceTypeDialogue,
	pattern AVAudioServiceTypeCommentary,
	pattern AVAudioServiceTypeEmergency,
	pattern AVAudioServiceTypeVoiceOver,
	pattern AVAudioServiceTypeKaraoke,

	AVFieldOrder,
	pattern AVFieldUnknown,
	pattern AVFieldProgressive,
	pattern AVFieldTt,
	pattern AVFieldBb,
	pattern AVFieldTb,
	pattern AVFieldBt,

	AVSubtitleType,
	pattern SubtitleNone,
	pattern SubtitleBitmap,
	pattern SubtitleText,
	pattern SubtitleAss,

	AVDiscard,
	pattern AVDiscardNone,
	pattern AVDiscardDefault,
	pattern AVDiscardNonref,
	pattern AVDiscardBidir,
	pattern AVDiscardNonintra,
	pattern AVDiscardNonkey,
	pattern AVDiscardAll,

	pattern FFInputBufferPaddingSize,

	Motion_Est_ID,
	pattern MEZero,
	pattern MEFull,
	pattern MELog,
	pattern MEPhods,
	pattern MEEpzs,
	pattern MEX1,
	pattern MEHex,
	pattern MEUmh,
	pattern METesa,
	pattern MEIter,

	pattern FFMinBufferSize,

	FFProfile,
	pattern FFProfileUnknown,
	pattern FFProfileReserved,
	pattern FFProfileAacMain,
	pattern FFProfileAacLow,
	pattern FFProfileAacSsr,
	pattern FFProfileAacLtp,
	pattern FFProfileAacHe,
	pattern FFProfileAacHeV2,
	pattern FFProfileAacLd,
	pattern FFProfileAacEld,
	pattern FFProfileMpeg2AacLow,
	pattern FFProfileMpeg2AacHe,
	pattern FFProfileDts,
	pattern FFProfileDtsEs,
	pattern FFProfileDts9624,
	pattern FFProfileDtsHdHra,
	pattern FFProfileDtsHdMa,
	pattern FFProfileMpeg2422,
	pattern FFProfileMpeg2High,
	pattern FFProfileMpeg2Ss,
	pattern FFProfileMpeg2SnrScalable,
	pattern FFProfileMpeg2Main,
	pattern FFProfileMpeg2Simple,
	pattern FFProfileH264Constrained,
	pattern FFProfileH264Intra,
	pattern FFProfileH264Baseline,
	pattern FFProfileH264ConstrainedBaseline,
	pattern FFProfileH264Main,
	pattern FFProfileH264Extended,
	pattern FFProfileH264High,
	pattern FFProfileH264High10,
	pattern FFProfileH264High10Intra,
	pattern FFProfileH264High422,
	pattern FFProfileH264High422Intra,
	pattern FFProfileH264High444,
	pattern FFProfileH264High444Predictive,
	pattern FFProfileH264High444Intra,
	pattern FFProfileH264Cavlc444,
	pattern FFProfileVc1Simple,
	pattern FFProfileVc1Main,
	pattern FFProfileVc1Complex,
	pattern FFProfileVc1Advanced,
	pattern FFProfileMpeg4Simple,
	pattern FFProfileMpeg4SimpleScalable,
	pattern FFProfileMpeg4Core,
	pattern FFProfileMpeg4Main,
	pattern FFProfileMpeg4NBit,
	pattern FFProfileMpeg4ScalableTexture,
	pattern FFProfileMpeg4SimpleFaceAnimation,
	pattern FFProfileMpeg4BasicAnimatedTexture,
	pattern FFProfileMpeg4Hybrid,
	pattern FFProfileMpeg4AdvancedRealTime,
	pattern FFProfileMpeg4CoreScalable,
	pattern FFProfileMpeg4AdvancedCoding,
	pattern FFProfileMpeg4AdvancedCore,
	pattern FFProfileMpeg4AdvancedScalableTexture,
	pattern FFProfileMpeg4SimpleStudio,
	pattern FFProfileMpeg4AdvancedSimple,
	pattern FFProfileJpeg2000CstreamRestriction0,
	pattern FFProfileJpeg2000CstreamRestriction1,
	pattern FFProfileJpeg2000CstreamNoRestriction,
	pattern FFProfileJpeg2000Dcinema2k,
	pattern FFProfileJpeg2000Dcinema4k,
	pattern FFProfileHevcMain,
	pattern FFProfileHevcMain10,
	pattern FFProfileHevcMainStillPicture,
	pattern FFProfileHevcRext,

	FFBug,
	pattern FFBugAutodetect,
	pattern FFBugOldMsmpeg4,
	pattern FFBugXvidIlace,
	pattern FFBugUmp4,
	pattern FFBugNoPadding,
	pattern FFBugAmv,
	pattern FFBugAcVlc,
	pattern FFBugQpelChroma,
	pattern FFBugStdQpel,
	pattern FFBugQpelChroma2,
	pattern FFBugDirectBlocksize,
	pattern FFBugEdge,
	pattern FFBugHpelChroma,
	pattern FFBugDcClip,
	pattern FFBugMs,
	pattern FFBugTruncated,

	FFDCT,
	pattern FFDCTAuto,
	pattern FFDCTFastint,
	pattern FFDCTInt,
	pattern FFDCTMmx,
	pattern FFDCTAltivec,
	pattern FFDCTFaan,

	FFIdct,
	pattern FFIdctAuto,
	pattern FFIdctInt,
	pattern FFIdctSimple,
	pattern FFIdctSimplemmx,
	pattern FFIdctArm,
	pattern FFIdctAltivec,
	pattern FFIdctSh4,
	pattern FFIdctSimplearm,
	pattern FFIdctIpp,
	pattern FFIdctXvid,
	pattern FFIdctXvidmmx,
	pattern FFIdctSimplearmv5te,
	pattern FFIdctSimplearmv6,
	pattern FFIdctSimplevis,
	pattern FFIdctFaan,
	pattern FFIdctSimpleneon,
	pattern FFIdctSimplealpha,
	pattern FFIdctSimpleauto,

	FFEC,
	pattern FFECGuessMvs,
	pattern FFECDeblock,
	pattern FFECFavorInter,

	FFPred,
	pattern FFPredLeft,
	pattern FFPredPlane,
	pattern FFPredMedian,

	FFDebug,
	pattern FFDebugPictInfo,
	pattern FFDebugRc,
	pattern FFDebugBitstream,
	pattern FFDebugMbType,
	pattern FFDebugQp,
	pattern FFDebugMv,
	pattern FFDebugDctCoeff,
	pattern FFDebugSkip,
	pattern FFDebugStartcode,
	pattern FFDebugPts,
	pattern FFDebugEr,
	pattern FFDebugMmco,
	pattern FFDebugBugs,
	pattern FFDebugVisQp,
	pattern FFDebugVisMbType,
	pattern FFDebugBuffers,
	pattern FFDebugThreads,
	--pattern FFDebugGreenMd,
	pattern FFDebugNomc,
	pattern FFDebugVisMvPFor,
	pattern FFDebugVisMvBFor,
	pattern FFDebugVisMvBBack,

	FFCoderType,
	pattern FFCoderTypeVlc,
	pattern FFCoderTypeAc,
	pattern FFCoderTypeRaw,
	pattern FFCoderTypeRle,
	pattern FFCoderTypeDeflate,

	FFMBDecision,
	pattern FFMBDecisionSimple,
	pattern FFMBDecisionBits,
	pattern FFMBDecisionRd,

	FFLevel,
	pattern FFLevelUnknown,

	FFSubCharencMode,
	pattern FFSubCharencModeDoNothing,
	pattern FFSubCharencModeAutomatic,
	pattern FFSubCharencModePreDecoder,

	FFThread,
	pattern FFThreadFrame,
	pattern FFThreadSlice,

	FFCmp,
	pattern FFCmpSad,
	pattern FFCmpSse,
	pattern FFCmpSatd,
	pattern FFCmpDct,
	pattern FFCmpPsnr,
	pattern FFCmpBit,
	pattern FFCmpRd,
	pattern FFCmpZero,
	pattern FFCmpVsad,
	pattern FFCmpVsse,
	pattern FFCmpNsse,
	pattern FFCmpW53,
	pattern FFCmpW97,
	pattern FFCmpDctmax,
	pattern FFCmpDct264,
	pattern FFCmpChroma,

	SliceFlags,
	pattern SliceFlagCodedOrder,
	pattern SliceFlagAllowField,
	pattern SliceFlagAllowPlane,

	AVCodecID,
	pattern AVCodecIdNone,
	pattern AVCodecIdMpeg1video,
	pattern AVCodecIdMpeg2video,
	pattern AVCodecIdH261,
	pattern AVCodecIdH263,
	pattern AVCodecIdRv10,
	pattern AVCodecIdRv20,
	pattern AVCodecIdMjpeg,
	pattern AVCodecIdMjpegb,
	pattern AVCodecIdLjpeg,
	pattern AVCodecIdSp5x,
	pattern AVCodecIdJpegls,
	pattern AVCodecIdMpeg4,
	pattern AVCodecIdRawvideo,
	pattern AVCodecIdMsmpeg4v1,
	pattern AVCodecIdMsmpeg4v2,
	pattern AVCodecIdMsmpeg4v3,
	pattern AVCodecIdWmv1,
	pattern AVCodecIdWmv2,
	pattern AVCodecIdH263p,
	pattern AVCodecIdH263i,
	pattern AVCodecIdFlv1,
	pattern AVCodecIdSvq1,
	pattern AVCodecIdSvq3,
	pattern AVCodecIdDvvideo,
	pattern AVCodecIdHuffyuv,
	pattern AVCodecIdCyuv,
	pattern AVCodecIdH264,
	pattern AVCodecIdIndeo3,
	pattern AVCodecIdVp3,
	pattern AVCodecIdTheora,
	pattern AVCodecIdAsv1,
	pattern AVCodecIdAsv2,
	pattern AVCodecIdFfv1,
	pattern AVCodecId4xm,
	pattern AVCodecIdVcr1,
	pattern AVCodecIdCljr,
	pattern AVCodecIdMdec,
	pattern AVCodecIdRoq,
	pattern AVCodecIdInterplayVideo,
	pattern AVCodecIdXanWc3,
	pattern AVCodecIdXanWc4,
	pattern AVCodecIdRpza,
	pattern AVCodecIdCinepak,
	pattern AVCodecIdWsVqa,
	pattern AVCodecIdMsrle,
	pattern AVCodecIdMsvideo1,
	pattern AVCodecIdIdcin,
	pattern AVCodecId8bps,
	pattern AVCodecIdSmc,
	pattern AVCodecIdFlic,
	pattern AVCodecIdTruemotion1,
	pattern AVCodecIdVmdvideo,
	pattern AVCodecIdMszh,
	pattern AVCodecIdZlib,
	pattern AVCodecIdQtrle,
	pattern AVCodecIdTscc,
	pattern AVCodecIdUlti,
	pattern AVCodecIdQdraw,
	pattern AVCodecIdVixl,
	pattern AVCodecIdQpeg,
	pattern AVCodecIdPng,
	pattern AVCodecIdPpm,
	pattern AVCodecIdPbm,
	pattern AVCodecIdPgm,
	pattern AVCodecIdPgmyuv,
	pattern AVCodecIdPam,
	pattern AVCodecIdFfvhuff,
	pattern AVCodecIdRv30,
	pattern AVCodecIdRv40,
	pattern AVCodecIdVc1,
	pattern AVCodecIdWmv3,
	pattern AVCodecIdLoco,
	pattern AVCodecIdWnv1,
	pattern AVCodecIdAasc,
	pattern AVCodecIdIndeo2,
	pattern AVCodecIdFraps,
	pattern AVCodecIdTruemotion2,
	pattern AVCodecIdBmp,
	pattern AVCodecIdCscd,
	pattern AVCodecIdMmvideo,
	pattern AVCodecIdZmbv,
	pattern AVCodecIdAvs,
	pattern AVCodecIdSmackvideo,
	pattern AVCodecIdNuv,
	pattern AVCodecIdKmvc,
	pattern AVCodecIdFlashsv,
	pattern AVCodecIdCavs,
	pattern AVCodecIdJpeg2000,
	pattern AVCodecIdVmnc,
	pattern AVCodecIdVp5,
	pattern AVCodecIdVp6,
	pattern AVCodecIdVp6f,
	pattern AVCodecIdTarga,
	pattern AVCodecIdDsicinvideo,
	pattern AVCodecIdTiertexseqvideo,
	pattern AVCodecIdTiff,
	pattern AVCodecIdGif,
	pattern AVCodecIdDxa,
	pattern AVCodecIdDnxhd,
	pattern AVCodecIdThp,
	pattern AVCodecIdSgi,
	pattern AVCodecIdC93,
	pattern AVCodecIdBethsoftvid,
	pattern AVCodecIdPtx,
	pattern AVCodecIdTxd,
	pattern AVCodecIdVp6a,
	pattern AVCodecIdAmv,
	pattern AVCodecIdVb,
	pattern AVCodecIdPcx,
	pattern AVCodecIdSunrast,
	pattern AVCodecIdIndeo4,
	pattern AVCodecIdIndeo5,
	pattern AVCodecIdMimic,
	pattern AVCodecIdRl2,
	pattern AVCodecIdEscape124,
	pattern AVCodecIdDirac,
	pattern AVCodecIdBfi,
	pattern AVCodecIdCmv,
	pattern AVCodecIdMotionpixels,
	pattern AVCodecIdTgv,
	pattern AVCodecIdTgq,
	pattern AVCodecIdTqi,
	pattern AVCodecIdAura,
	pattern AVCodecIdAura2,
	pattern AVCodecIdV210x,
	pattern AVCodecIdTmv,
	pattern AVCodecIdV210,
	pattern AVCodecIdDpx,
	pattern AVCodecIdMad,
	pattern AVCodecIdFrwu,
	pattern AVCodecIdFlashsv2,
	pattern AVCodecIdCdgraphics,
	pattern AVCodecIdR210,
	pattern AVCodecIdAnm,
	pattern AVCodecIdBinkvideo,
	pattern AVCodecIdIffIlbm,
	pattern AVCodecIdIffByterun1,
	pattern AVCodecIdKgv1,
	pattern AVCodecIdYop,
	pattern AVCodecIdVp8,
	pattern AVCodecIdPictor,
	pattern AVCodecIdAnsi,
	pattern AVCodecIdA64Multi,
	pattern AVCodecIdA64Multi5,
	pattern AVCodecIdR10k,
	pattern AVCodecIdMxpeg,
	pattern AVCodecIdLagarith,
	pattern AVCodecIdProres,
	pattern AVCodecIdJv,
	pattern AVCodecIdDfa,
	pattern AVCodecIdWmv3image,
	pattern AVCodecIdVc1image,
	pattern AVCodecIdUtvideo,
	pattern AVCodecIdBmvVideo,
	pattern AVCodecIdVble,
	pattern AVCodecIdDxtory,
	pattern AVCodecIdV410,
	pattern AVCodecIdXwd,
	pattern AVCodecIdCdxl,
	pattern AVCodecIdXbm,
	pattern AVCodecIdZerocodec,
	pattern AVCodecIdMss1,
	pattern AVCodecIdMsa1,
	pattern AVCodecIdTscc2,
	pattern AVCodecIdMts2,
	pattern AVCodecIdCllc,
	pattern AVCodecIdMss2,
	pattern AVCodecIdVp9,
	pattern AVCodecIdAic,
	pattern AVCodecIdEscape130Deprecated,
	pattern AVCodecIdG2mDeprecated,
	pattern AVCodecIdWebpDeprecated,
	pattern AVCodecIdHnm4Video,
	pattern AVCodecIdHevcDeprecated,
	pattern AVCodecIdFic,
	pattern AVCodecIdAliasPix,
	pattern AVCodecIdBrenderPixDeprecated,
	pattern AVCodecIdPafVideoDeprecated,
	pattern AVCodecIdExrDeprecated,
	pattern AVCodecIdVp7Deprecated,
	pattern AVCodecIdSanmDeprecated,
	pattern AVCodecIdSgirleDeprecated,
	pattern AVCodecIdMvc1Deprecated,
	pattern AVCodecIdMvc2Deprecated,
	pattern AVCodecIdBrenderPix,
	pattern AVCodecIdY41p,
	pattern AVCodecIdEscape130,
	pattern AVCodecIdExr,
	pattern AVCodecIdAvrp,
	pattern AVCodecId012v,
	pattern AVCodecIdG2m,
	pattern AVCodecIdAvui,
	pattern AVCodecIdAyuv,
	pattern AVCodecIdTargaY216,
	pattern AVCodecIdV308,
	pattern AVCodecIdV408,
	pattern AVCodecIdYuv4,
	pattern AVCodecIdSanm,
	pattern AVCodecIdPafVideo,
	pattern AVCodecIdAvrn,
	pattern AVCodecIdCpia,
	pattern AVCodecIdXface,
	pattern AVCodecIdSgirle,
	pattern AVCodecIdMvc1,
	pattern AVCodecIdMvc2,
	pattern AVCodecIdSnow,
	pattern AVCodecIdWebp,
	pattern AVCodecIdSmvjpeg,
	pattern AVCodecIdHevc,
	pattern AVCodecIdH265,
	pattern AVCodecIdVp7,
	pattern AVCodecIdApng,
	pattern AVCodecIdFirstAudio,
	pattern AVCodecIdPcmS16le,
	pattern AVCodecIdPcmS16be,
	pattern AVCodecIdPcmU16le,
	pattern AVCodecIdPcmU16be,
	pattern AVCodecIdPcmS8,
	pattern AVCodecIdPcmU8,
	pattern AVCodecIdPcmMulaw,
	pattern AVCodecIdPcmAlaw,
	pattern AVCodecIdPcmS32le,
	pattern AVCodecIdPcmS32be,
	pattern AVCodecIdPcmU32le,
	pattern AVCodecIdPcmU32be,
	pattern AVCodecIdPcmS24le,
	pattern AVCodecIdPcmS24be,
	pattern AVCodecIdPcmU24le,
	pattern AVCodecIdPcmU24be,
	pattern AVCodecIdPcmS24daud,
	pattern AVCodecIdPcmZork,
	pattern AVCodecIdPcmS16lePlanar,
	pattern AVCodecIdPcmDvd,
	pattern AVCodecIdPcmF32be,
	pattern AVCodecIdPcmF32le,
	pattern AVCodecIdPcmF64be,
	pattern AVCodecIdPcmF64le,
	pattern AVCodecIdPcmBluray,
	pattern AVCodecIdPcmLxf,
	pattern AVCodecIdS302m,
	pattern AVCodecIdPcmS8Planar,
	pattern AVCodecIdPcmS24lePlanarDeprecated,
	pattern AVCodecIdPcmS32lePlanarDeprecated,
	pattern AVCodecIdPcmS24lePlanar,
	pattern AVCodecIdPcmS32lePlanar,
	pattern AVCodecIdPcmS16bePlanar,
	pattern AVCodecIdAdpcmImaQt,
	pattern AVCodecIdAdpcmImaWav,
	pattern AVCodecIdAdpcmImaDk3,
	pattern AVCodecIdAdpcmImaDk4,
	pattern AVCodecIdAdpcmImaWs,
	pattern AVCodecIdAdpcmImaSmjpeg,
	pattern AVCodecIdAdpcmMs,
	pattern AVCodecIdAdpcm4xm,
	pattern AVCodecIdAdpcmXa,
	pattern AVCodecIdAdpcmAdx,
	pattern AVCodecIdAdpcmEa,
	pattern AVCodecIdAdpcmG726,
	pattern AVCodecIdAdpcmCt,
	pattern AVCodecIdAdpcmSwf,
	pattern AVCodecIdAdpcmYamaha,
	pattern AVCodecIdAdpcmSbpro4,
	pattern AVCodecIdAdpcmSbpro3,
	pattern AVCodecIdAdpcmSbpro2,
	pattern AVCodecIdAdpcmThp,
	pattern AVCodecIdAdpcmImaAmv,
	pattern AVCodecIdAdpcmEaR1,
	pattern AVCodecIdAdpcmEaR3,
	pattern AVCodecIdAdpcmEaR2,
	pattern AVCodecIdAdpcmImaEaSead,
	pattern AVCodecIdAdpcmImaEaEacs,
	pattern AVCodecIdAdpcmEaXas,
	pattern AVCodecIdAdpcmEaMaxisXa,
	pattern AVCodecIdAdpcmImaIss,
	pattern AVCodecIdAdpcmG722,
	pattern AVCodecIdAdpcmImaApc,
	pattern AVCodecIdAdpcmVimaDeprecated,
	pattern AVCodecIdAdpcmVima,
	pattern AVCodecIdAdpcmAfc,
	pattern AVCodecIdAdpcmImaOki,
	pattern AVCodecIdAdpcmDtk,
	pattern AVCodecIdAdpcmImaRad,
	pattern AVCodecIdAdpcmG726le,
	pattern AVCodecIdAmrNb,
	pattern AVCodecIdAmrWb,
	pattern AVCodecIdRa144,
	pattern AVCodecIdRa288,
	pattern AVCodecIdRoqDpcm,
	pattern AVCodecIdInterplayDpcm,
	pattern AVCodecIdXanDpcm,
	pattern AVCodecIdSolDpcm,
	pattern AVCodecIdMp2,
	pattern AVCodecIdMp3,
	pattern AVCodecIdAac,
	pattern AVCodecIdAc3,
	pattern AVCodecIdDts,
	pattern AVCodecIdVorbis,
	pattern AVCodecIdDvaudio,
	pattern AVCodecIdWmav1,
	pattern AVCodecIdWmav2,
	pattern AVCodecIdMace3,
	pattern AVCodecIdMace6,
	pattern AVCodecIdVmdaudio,
	pattern AVCodecIdFlac,
	pattern AVCodecIdMp3adu,
	pattern AVCodecIdMp3on4,
	pattern AVCodecIdShorten,
	pattern AVCodecIdAlac,
	pattern AVCodecIdWestwoodSnd1,
	pattern AVCodecIdGsm,
	pattern AVCodecIdQdm2,
	pattern AVCodecIdCook,
	pattern AVCodecIdTruespeech,
	pattern AVCodecIdTta,
	pattern AVCodecIdSmackaudio,
	pattern AVCodecIdQcelp,
	pattern AVCodecIdWavpack,
	pattern AVCodecIdDsicinaudio,
	pattern AVCodecIdImc,
	pattern AVCodecIdMusepack7,
	pattern AVCodecIdMlp,
	pattern AVCodecIdGsmMs,
	pattern AVCodecIdAtrac3,
	pattern AVCodecIdApe,
	pattern AVCodecIdNellymoser,
	pattern AVCodecIdMusepack8,
	pattern AVCodecIdSpeex,
	pattern AVCodecIdWmavoice,
	pattern AVCodecIdWmapro,
	pattern AVCodecIdWmalossless,
	pattern AVCodecIdAtrac3p,
	pattern AVCodecIdEac3,
	pattern AVCodecIdSipr,
	pattern AVCodecIdMp1,
	pattern AVCodecIdTwinvq,
	pattern AVCodecIdTruehd,
	pattern AVCodecIdMp4als,
	pattern AVCodecIdAtrac1,
	pattern AVCodecIdBinkaudioRdft,
	pattern AVCodecIdBinkaudioDct,
	pattern AVCodecIdAacLatm,
	pattern AVCodecIdQdmc,
	pattern AVCodecIdCelt,
	pattern AVCodecIdG723_1,
	pattern AVCodecIdG729,
	pattern AVCodecId8svxExp,
	pattern AVCodecId8svxFib,
	pattern AVCodecIdBmvAudio,
	pattern AVCodecIdRalf,
	pattern AVCodecIdIac,
	pattern AVCodecIdIlbc,
	pattern AVCodecIdOpusDeprecated,
	pattern AVCodecIdComfortNoise,
	pattern AVCodecIdTakDeprecated,
	pattern AVCodecIdMetasound,
	pattern AVCodecIdPafAudioDeprecated,
	pattern AVCodecIdOn2avc,
	pattern AVCodecIdFfwavesynth,
	pattern AVCodecIdSonic,
	pattern AVCodecIdSonicLs,
	pattern AVCodecIdPafAudio,
	pattern AVCodecIdOpus,
	pattern AVCodecIdTak,
	pattern AVCodecIdEvrc,
	pattern AVCodecIdSmv,
	pattern AVCodecIdDsdLsbf,
	pattern AVCodecIdDsdMsbf,
	pattern AVCodecIdDsdLsbfPlanar,
	pattern AVCodecIdDsdMsbfPlanar,
	pattern AVCodecIdFirstSubtitle,
	pattern AVCodecIdDvdSubtitle,
	pattern AVCodecIdDvbSubtitle,
	pattern AVCodecIdText,
	pattern AVCodecIdXsub,
	pattern AVCodecIdSsa,
	pattern AVCodecIdMovText,
	pattern AVCodecIdHdmvPgsSubtitle,
	pattern AVCodecIdDvbTeletext,
	pattern AVCodecIdSrt,
	pattern AVCodecIdMicrodvd,
	pattern AVCodecIdEia608,
	pattern AVCodecIdJacosub,
	pattern AVCodecIdSami,
	pattern AVCodecIdRealtext,
	pattern AVCodecIdStl,
	pattern AVCodecIdSubviewer1,
	pattern AVCodecIdSubviewer,
	pattern AVCodecIdSubrip,
	pattern AVCodecIdWebvtt,
	pattern AVCodecIdMpl2,
	pattern AVCodecIdVplayer,
	pattern AVCodecIdPjs,
	pattern AVCodecIdAss,
	pattern AVCodecIdFirstUnknown,
	pattern AVCodecIdTtf,
	pattern AVCodecIdBintext,
	pattern AVCodecIdXbin,
	pattern AVCodecIdIdf,
	pattern AVCodecIdOtf,
	pattern AVCodecIdSmpteKlv,
	pattern AVCodecIdDvdNav,
	pattern AVCodecIdTimedId3,
	pattern AVCodecIdBinData,
	pattern AVCodecIdProbe,
	pattern AVCodecIdMpeg2ts,
	pattern AVCodecIdMpeg4systems,
	pattern AVCodecIdFfmetadata
) where

#include "ffmpeg.h"

import Data.Word
import Foreign.C.Types
import Foreign.Storable

import Media.FFMpeg.Internal.Common

-- | AVMediaType
newtype AVMediaType = AVMediaType CInt deriving (Eq, Show, CEnum, Storable)
pattern AVMediaTypeUnknown = AVMediaType (#{const AVMEDIA_TYPE_UNKNOWN})
pattern AVMediaTypeVideo = AVMediaType #{const AVMEDIA_TYPE_VIDEO}
pattern AVMediaTypeAudio = AVMediaType #{const AVMEDIA_TYPE_AUDIO}
pattern AVMediaTypeData = AVMediaType #{const AVMEDIA_TYPE_DATA}
pattern AVMediaTypeSubtitle = AVMediaType #{const AVMEDIA_TYPE_SUBTITLE}
pattern AVMediaTypeAttachment = AVMediaType #{const AVMEDIA_TYPE_ATTACHMENT}

-- | AVSampleFormat
newtype AVSampleFormat = AVSampleFormat CInt deriving (Eq, Show, CEnum, Storable)
pattern AVSampleFmtNone = AVSampleFormat (#{const AV_SAMPLE_FMT_NONE})
pattern AVSampleFmtU8 = AVSampleFormat #{const AV_SAMPLE_FMT_U8}
pattern AVSampleFmtS16 = AVSampleFormat #{const AV_SAMPLE_FMT_S16}
pattern AVSampleFmtS32 = AVSampleFormat #{const AV_SAMPLE_FMT_S32}
pattern AVSampleFmtFlt = AVSampleFormat #{const AV_SAMPLE_FMT_FLT}
pattern AVSampleFmtDbl = AVSampleFormat #{const AV_SAMPLE_FMT_DBL}
pattern AVSampleFmtU8p = AVSampleFormat #{const AV_SAMPLE_FMT_U8P}
pattern AVSampleFmtS16p = AVSampleFormat #{const AV_SAMPLE_FMT_S16P}
pattern AVSampleFmtS32p = AVSampleFormat #{const AV_SAMPLE_FMT_S32P}
pattern AVSampleFmtFltp = AVSampleFormat #{const AV_SAMPLE_FMT_FLTP}
pattern AVSampleFmtDblp = AVSampleFormat #{const AV_SAMPLE_FMT_DBLP}

-- | AVPacketSideDataType
newtype AVPacketSideDataType = AVPacketSideDataType CInt deriving (Eq, Show, CEnum, Storable)
pattern AVPktDataPalette = AVPacketSideDataType #{const AV_PKT_DATA_PALETTE}
pattern AVPktDataNewExtradata = AVPacketSideDataType #{const AV_PKT_DATA_NEW_EXTRADATA}
pattern AVPktDataParamChange = AVPacketSideDataType #{const AV_PKT_DATA_PARAM_CHANGE}
pattern AVPktDataH263MbInfo = AVPacketSideDataType #{const AV_PKT_DATA_H263_MB_INFO}
pattern AVPktDataReplayGain = AVPacketSideDataType #{const AV_PKT_DATA_REPLAYGAIN}
pattern AVPktDataDisplayMatrix = AVPacketSideDataType #{const AV_PKT_DATA_DISPLAYMATRIX}
pattern AVPktDataStereo3d = AVPacketSideDataType #{const AV_PKT_DATA_STEREO3D}
pattern AVPktDataSkipSamples = AVPacketSideDataType #{const AV_PKT_DATA_SKIP_SAMPLES}
pattern AVPktDataJpDualmono = AVPacketSideDataType #{const AV_PKT_DATA_JP_DUALMONO}
pattern AVPktDataStringsMetadata = AVPacketSideDataType #{const AV_PKT_DATA_STRINGS_METADATA}
pattern AVPktDataSubtitlePosition = AVPacketSideDataType #{const AV_PKT_DATA_SUBTITLE_POSITION}
pattern AVPktDataMatroskaBlockadditional = AVPacketSideDataType #{const AV_PKT_DATA_MATROSKA_BLOCKADDITIONAL}
pattern AVPktDataWebvttIdentifier = AVPacketSideDataType #{const AV_PKT_DATA_WEBVTT_IDENTIFIER}
pattern AVPktDataWebvttSettings = AVPacketSideDataType #{const AV_PKT_DATA_WEBVTT_SETTINGS}
pattern AVPktDataMetadataUpdate = AVPacketSideDataType #{const AV_PKT_DATA_METADATA_UPDATE}

--av_pkt_data_audio_service_type = AV_PKT_DATA_AUDIO_SERVICE_TYPE,
--av_pkt_data_quality_factor = AV_PKT_DATA_QUALITY_FACTOR,

-- | AV_PKT_FLAG_ flags
newtype AVPacketFlag = AVPacketFlag CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVPktFlagKey = AVPacketFlag #{const AV_PKT_FLAG_KEY}
pattern AVPktFlagCorrupt = AVPacketFlag #{const AV_PKT_FLAG_CORRUPT}

-- | AVSideDataParamChangeFlags
newtype AVSideDataParamChangeFlags = AVSideDataParamChangeFlags Word32 deriving (Eq, Show, Storable)
pattern AVSideDataParamChangeChannelCount = AVSideDataParamChangeFlags #{const AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_COUNT}
pattern AVSideDataParamChangeChannelLayout = AVSideDataParamChangeFlags #{const AV_SIDE_DATA_PARAM_CHANGE_CHANNEL_LAYOUT}
pattern AVSideDataParamChangeSampleRate = AVSideDataParamChangeFlags #{const AV_SIDE_DATA_PARAM_CHANGE_SAMPLE_RATE}
pattern AVSideDataParamChangeDimensions = AVSideDataParamChangeFlags #{const AV_SIDE_DATA_PARAM_CHANGE_DIMENSIONS}

-- | AV_CODEC_PROP_ flags
newtype AVCodecProp = AVCodecProp CInt deriving (Eq, Show, CEnum, CFlags)
pattern AVCodecPropIntraOnly = AVCodecProp #{const AV_CODEC_PROP_INTRA_ONLY}
pattern AVCodecPropLossy = AVCodecProp #{const AV_CODEC_PROP_LOSSY}
pattern AVCodecPropLossless = AVCodecProp #{const AV_CODEC_PROP_LOSSLESS}
pattern AVCodecPropReorder = AVCodecProp #{const AV_CODEC_PROP_REORDER}
pattern AVCodecPropBitmapSub = AVCodecProp #{const AV_CODEC_PROP_BITMAP_SUB}
pattern AVCodecPropTextSub = AVCodecProp #{const AV_CODEC_PROP_TEXT_SUB}

-- | FF_MAX_B_FRAMES
pattern FFMaxBFrames = #{const FF_MAX_B_FRAMES}

-- | FF_COMPRESSION_DEFAULT
pattern FFCompressionDefault = (#{const FF_COMPRESSION_DEFAULT})

-- | CODEC_FLAG_ flags
newtype AVCodecFlag = AVCodecFlag CInt deriving (Eq, Show, CFlags)
pattern CodecFlagUnaligned = AVCodecFlag #{const CODEC_FLAG_UNALIGNED}
pattern CodecFlagQscale = AVCodecFlag #{const CODEC_FLAG_QSCALE}
pattern CodecFlag4mv = AVCodecFlag #{const CODEC_FLAG_4MV}
pattern CodecFlagOutputCorrupt = AVCodecFlag #{const CODEC_FLAG_OUTPUT_CORRUPT}
pattern CodecFlagQpel = AVCodecFlag #{const CODEC_FLAG_QPEL}
pattern CodecFlagGmc = AVCodecFlag #{const CODEC_FLAG_GMC}
pattern CodecFlagMv0 = AVCodecFlag #{const CODEC_FLAG_MV0}
pattern CodecFlagInputPreserved = AVCodecFlag #{const CODEC_FLAG_INPUT_PRESERVED}
pattern CodecFlagPass1 = AVCodecFlag #{const CODEC_FLAG_PASS1}
pattern CodecFlagPass2 = AVCodecFlag #{const CODEC_FLAG_PASS2}
pattern CodecFlagGray = AVCodecFlag #{const CODEC_FLAG_GRAY}
pattern CodecFlagEmuEdge = AVCodecFlag #{const CODEC_FLAG_EMU_EDGE}
pattern CodecFlagPsnr = AVCodecFlag #{const CODEC_FLAG_PSNR}
pattern CodecFlagTruncated = AVCodecFlag #{const CODEC_FLAG_TRUNCATED}
pattern CodecFlagNormalizeAqp = AVCodecFlag #{const CODEC_FLAG_NORMALIZE_AQP}
pattern CodecFlagInterlacedDct = AVCodecFlag #{const CODEC_FLAG_INTERLACED_DCT}
pattern CodecFlagLowDelay = AVCodecFlag #{const CODEC_FLAG_LOW_DELAY}
pattern CodecFlagGlobalHeader = AVCodecFlag #{const CODEC_FLAG_GLOBAL_HEADER}
pattern CodecFlagBitexact = AVCodecFlag #{const CODEC_FLAG_BITEXACT}
pattern CodecFlagAcPred = AVCodecFlag #{const CODEC_FLAG_AC_PRED}
pattern CodecFlagLoopFilter = AVCodecFlag #{const CODEC_FLAG_LOOP_FILTER}
pattern CodecFlagInterlacedMe = AVCodecFlag #{const CODEC_FLAG_INTERLACED_ME}
pattern CodecFlagClosedGop = AVCodecFlag #{const CODEC_FLAG_CLOSED_GOP}

-- | CODEC_FLAG2_ flags
newtype AVCodecFlag2 = AVCodecFlag2 CInt deriving (Eq, Show, CFlags)
pattern CodecFlag2Fast = AVCodecFlag2 #{const CODEC_FLAG2_FAST}
pattern CodecFlag2NoOutput = AVCodecFlag2 #{const CODEC_FLAG2_NO_OUTPUT}
pattern CodecFlag2LocalHeader = AVCodecFlag2 #{const CODEC_FLAG2_LOCAL_HEADER}
pattern CodecFlag2DropFrameTimecode = AVCodecFlag2 #{const CODEC_FLAG2_DROP_FRAME_TIMECODE}
pattern CodecFlag2IgnoreCrop = AVCodecFlag2 #{const CODEC_FLAG2_IGNORE_CROP}
pattern CodecFlag2Chunks = AVCodecFlag2 #{const CODEC_FLAG2_CHUNKS}
pattern CodecFlag2ShowAll = AVCodecFlag2 #{const CODEC_FLAG2_SHOW_ALL}
pattern CodecFlag2ExportMvs = AVCodecFlag2 #{const CODEC_FLAG2_EXPORT_MVS}
pattern CodecFlag2SkipManual = AVCodecFlag2 #{const CODEC_FLAG2_SKIP_MANUAL}

-- | CODEC_CAP_ flags
newtype AVCodecCap = AVCodecCap CInt deriving (Eq, Show, CFlags)
pattern CodecCapDrawHorizBand = AVCodecCap #{const CODEC_CAP_DRAW_HORIZ_BAND}
pattern CodecCapDr1 = AVCodecCap #{const CODEC_CAP_DR1}
pattern CodecCapTruncated = AVCodecCap #{const CODEC_CAP_TRUNCATED}
pattern CodecCapDelay = AVCodecCap #{const CODEC_CAP_DELAY}
pattern CodecCapSmallLastFrame = AVCodecCap #{const CODEC_CAP_SMALL_LAST_FRAME}
pattern CodecCapHwaccelVdpau = AVCodecCap #{const CODEC_CAP_HWACCEL_VDPAU}
pattern CodecCapSubframes = AVCodecCap #{const CODEC_CAP_SUBFRAMES}
pattern CodecCapExperimental = AVCodecCap #{const CODEC_CAP_EXPERIMENTAL}
pattern CodecCapChannelConf = AVCodecCap #{const CODEC_CAP_CHANNEL_CONF}
pattern CodecCapNegLinesizes = AVCodecCap #{const CODEC_CAP_NEG_LINESIZES}
pattern CodecCapFrameThreads = AVCodecCap #{const CODEC_CAP_FRAME_THREADS}
pattern CodecCapSliceThreads = AVCodecCap #{const CODEC_CAP_SLICE_THREADS}
pattern CodecCapParamChange = AVCodecCap #{const CODEC_CAP_PARAM_CHANGE}
pattern CodecCapAutoThreads = AVCodecCap #{const CODEC_CAP_AUTO_THREADS}
pattern CodecCapVariableFrameSize = AVCodecCap #{const CODEC_CAP_VARIABLE_FRAME_SIZE}
pattern CodecCapIntraOnly = AVCodecCap #{const CODEC_CAP_INTRA_ONLY}
pattern CodecCapLossless = AVCodecCap #{const CODEC_CAP_LOSSLESS}

-- | MB_TYPE_ flags
newtype AVMBType = AVMBType CInt deriving (Eq, Show, CFlags)
pattern MBTypeIntra4X4 = AVMBType #{const MB_TYPE_INTRA4x4}
pattern MBTypeIntra16X16 = AVMBType #{const MB_TYPE_INTRA16x16}
pattern MBTypeIntraPcm = AVMBType #{const MB_TYPE_INTRA_PCM}
pattern MBType16X16 = AVMBType #{const MB_TYPE_16x16}
pattern MBType16X8 = AVMBType #{const MB_TYPE_16x8}
pattern MBType8X16 = AVMBType #{const MB_TYPE_8x16}
pattern MBType8X8 = AVMBType #{const MB_TYPE_8x8}
pattern MBTypeInterlaced = AVMBType #{const MB_TYPE_INTERLACED}
pattern MBTypeDirect2 = AVMBType #{const MB_TYPE_DIRECT2}
pattern MBTypeAcpred = AVMBType #{const MB_TYPE_ACPRED}
pattern MBTypeGmc = AVMBType #{const MB_TYPE_GMC}
pattern MBTypeSkip = AVMBType #{const MB_TYPE_SKIP}
pattern MBTypeP0l0 = AVMBType #{const MB_TYPE_P0L0}
pattern MBTypeP1l0 = AVMBType #{const MB_TYPE_P1L0}
pattern MBTypeP0l1 = AVMBType #{const MB_TYPE_P0L1}
pattern MBTypeP1l1 = AVMBType #{const MB_TYPE_P1L1}
pattern MBTypeL0 = AVMBType #{const MB_TYPE_L0}
pattern MBTypeL1 = AVMBType #{const MB_TYPE_L1}
pattern MBTypeL0l1 = AVMBType #{const MB_TYPE_L0L1}
pattern MBTypeQuant = AVMBType #{const MB_TYPE_QUANT}
pattern MBTypeCbp = AVMBType #{const MB_TYPE_CBP}

-- | FF_QSCALE_TYPE_ constants
newtype FFQscaleType = FFQscaleType CInt deriving (Eq, Show, CEnum)
pattern FFQscaleTypeMpeg1 = FFQscaleType #{const FF_QSCALE_TYPE_MPEG1}
pattern FFQscaleTypeMpeg2 = FFQscaleType #{const FF_QSCALE_TYPE_MPEG2}
pattern FFQscaleTypeH264 = FFQscaleType #{const FF_QSCALE_TYPE_H264}
pattern FFQscaleTypeVp56 = FFQscaleType #{const FF_QSCALE_TYPE_VP56}

--  | FF_BUFFER_TYPE_ constants
newtype FFBufferType = FFBufferType CInt deriving (Eq, Show, CEnum)
pattern FFBufferTypeInternal = FFBufferType #{const FF_BUFFER_TYPE_INTERNAL}
pattern FFBufferTypeUser = FFBufferType #{const FF_BUFFER_TYPE_USER}
pattern FFBufferTypeShared = FFBufferType #{const FF_BUFFER_TYPE_SHARED}
pattern FFBufferTypeCopy = FFBufferType #{const FF_BUFFER_TYPE_COPY}

-- | FF_BUFFER_HINTS_ flags
newtype FFBufferHints = FFBufferHints CInt deriving (Eq, Show, CFlags)
pattern FFBufferHintsValid = FFBufferHints #{const FF_BUFFER_HINTS_VALID}
pattern FFBufferHintsReadable = FFBufferHints #{const FF_BUFFER_HINTS_READABLE}
pattern FFBufferHintsPreserve = FFBufferHints #{const FF_BUFFER_HINTS_PRESERVE}
pattern FFBufferHintsReusable = FFBufferHints #{const FF_BUFFER_HINTS_REUSABLE}

-- | AV_GET_BUFFER_FLAG_ flags
newtype AVGetBufferFlag = AVGetBufferFlag CInt deriving (Eq, Show, CFlags)
pattern AVGetBufferFlagRef = AVGetBufferFlag #{const AV_GET_BUFFER_FLAG_REF}

-- | AV_SUBTITLE_FLAG_ flags
newtype AVSubtitleFlag = AVSubtitleFlag CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern AVSubtitleFlagForced = AVSubtitleFlag #{const AV_SUBTITLE_FLAG_FORCED}

-- | AVAudioServiceType
newtype AVAudioServiceType = AVAudioServiceType CInt deriving (Eq, Show, CEnum, Storable)
pattern AVAudioServiceTypeMain = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_MAIN}
pattern AVAudioServiceTypeEffects = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_EFFECTS}
pattern AVAudioServiceTypeVisuallyImpaired = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_VISUALLY_IMPAIRED}
pattern AVAudioServiceTypeHearingImpaired = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_HEARING_IMPAIRED}
pattern AVAudioServiceTypeDialogue = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_DIALOGUE}
pattern AVAudioServiceTypeCommentary = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_COMMENTARY}
pattern AVAudioServiceTypeEmergency = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_EMERGENCY}
pattern AVAudioServiceTypeVoiceOver = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_VOICE_OVER}
pattern AVAudioServiceTypeKaraoke = AVAudioServiceType #{const AV_AUDIO_SERVICE_TYPE_KARAOKE}

-- | AVFieldOrder
newtype AVFieldOrder = AVFieldOrder CInt deriving (Eq, Show, CEnum)
pattern AVFieldUnknown = AVFieldOrder #{const AV_FIELD_UNKNOWN}
pattern AVFieldProgressive = AVFieldOrder #{const AV_FIELD_PROGRESSIVE}
pattern AVFieldTt = AVFieldOrder #{const AV_FIELD_TT}
pattern AVFieldBb = AVFieldOrder #{const AV_FIELD_BB}
pattern AVFieldTb = AVFieldOrder #{const AV_FIELD_TB}
pattern AVFieldBt = AVFieldOrder #{const AV_FIELD_BT}

-- | AVSubtitleType
newtype AVSubtitleType = AVSubtitleType CInt deriving (Eq, Show, CEnum, Storable)
pattern SubtitleNone = AVSubtitleType #{const SUBTITLE_NONE}
pattern SubtitleBitmap = AVSubtitleType #{const SUBTITLE_BITMAP}
pattern SubtitleText = AVSubtitleType #{const SUBTITLE_TEXT}
pattern SubtitleAss = AVSubtitleType #{const SUBTITLE_ASS}

-- | AVDiscard
newtype AVDiscard = AVDiscard CInt deriving (Eq, Show, CEnum)
pattern AVDiscardNone = AVDiscard (#{const AVDISCARD_NONE})
pattern AVDiscardDefault = AVDiscard #{const AVDISCARD_DEFAULT}
pattern AVDiscardNonref = AVDiscard #{const AVDISCARD_NONREF}
pattern AVDiscardBidir = AVDiscard #{const AVDISCARD_BIDIR}
pattern AVDiscardNonintra = AVDiscard #{const AVDISCARD_NONINTRA}
pattern AVDiscardNonkey = AVDiscard #{const AVDISCARD_NONKEY}
pattern AVDiscardAll = AVDiscard #{const AVDISCARD_ALL}

-- | FF_INPUT_BUFFER_PADDING_SIZE
pattern FFInputBufferPaddingSize = #{const FF_INPUT_BUFFER_PADDING_SIZE}

-- | Motion_Est_ID
newtype Motion_Est_ID = Motion_Est_ID CInt deriving (Eq, Show, CEnum)
pattern MEZero = Motion_Est_ID #{const ME_ZERO}
pattern MEFull = Motion_Est_ID #{const ME_FULL}
pattern MELog = Motion_Est_ID #{const ME_LOG}
pattern MEPhods = Motion_Est_ID #{const ME_PHODS}
pattern MEEpzs = Motion_Est_ID #{const ME_EPZS}
pattern MEX1 = Motion_Est_ID #{const ME_X1}
pattern MEHex = Motion_Est_ID #{const ME_HEX}
pattern MEUmh = Motion_Est_ID #{const ME_UMH}
pattern METesa = Motion_Est_ID #{const ME_TESA}
pattern MEIter = Motion_Est_ID #{const ME_ITER}

-- | FF_MIN_BUFFER_SIZE
pattern FFMinBufferSize = #{const FF_MIN_BUFFER_SIZE}

-- | FF_PROFILE_ constants
newtype FFProfile = FFProfile CInt deriving (Eq, Show, CEnum, Storable)
pattern FFProfileUnknown = FFProfile (#{const FF_PROFILE_UNKNOWN})
pattern FFProfileReserved = FFProfile (#{const FF_PROFILE_RESERVED})
pattern FFProfileAacMain = FFProfile #{const FF_PROFILE_AAC_MAIN}
pattern FFProfileAacLow = FFProfile #{const FF_PROFILE_AAC_LOW}
pattern FFProfileAacSsr = FFProfile #{const FF_PROFILE_AAC_SSR}
pattern FFProfileAacLtp = FFProfile #{const FF_PROFILE_AAC_LTP}
pattern FFProfileAacHe = FFProfile #{const FF_PROFILE_AAC_HE}
pattern FFProfileAacHeV2 = FFProfile #{const FF_PROFILE_AAC_HE_V2}
pattern FFProfileAacLd = FFProfile #{const FF_PROFILE_AAC_LD}
pattern FFProfileAacEld = FFProfile #{const FF_PROFILE_AAC_ELD}
pattern FFProfileMpeg2AacLow = FFProfile #{const FF_PROFILE_MPEG2_AAC_LOW}
pattern FFProfileMpeg2AacHe = FFProfile #{const FF_PROFILE_MPEG2_AAC_HE}
pattern FFProfileDts = FFProfile #{const FF_PROFILE_DTS}
pattern FFProfileDtsEs = FFProfile #{const FF_PROFILE_DTS_ES}
pattern FFProfileDts9624 = FFProfile #{const FF_PROFILE_DTS_96_24}
pattern FFProfileDtsHdHra = FFProfile #{const FF_PROFILE_DTS_HD_HRA}
pattern FFProfileDtsHdMa = FFProfile #{const FF_PROFILE_DTS_HD_MA}
pattern FFProfileMpeg2422 = FFProfile #{const FF_PROFILE_MPEG2_422}
pattern FFProfileMpeg2High = FFProfile #{const FF_PROFILE_MPEG2_HIGH}
pattern FFProfileMpeg2Ss = FFProfile #{const FF_PROFILE_MPEG2_SS}
pattern FFProfileMpeg2SnrScalable = FFProfile #{const FF_PROFILE_MPEG2_SNR_SCALABLE}
pattern FFProfileMpeg2Main = FFProfile #{const FF_PROFILE_MPEG2_MAIN}
pattern FFProfileMpeg2Simple = FFProfile #{const FF_PROFILE_MPEG2_SIMPLE}
pattern FFProfileH264Constrained = FFProfile #{const FF_PROFILE_H264_CONSTRAINED}
pattern FFProfileH264Intra = FFProfile #{const FF_PROFILE_H264_INTRA}
pattern FFProfileH264Baseline = FFProfile #{const FF_PROFILE_H264_BASELINE}
pattern FFProfileH264ConstrainedBaseline = FFProfile #{const FF_PROFILE_H264_CONSTRAINED_BASELINE}
pattern FFProfileH264Main = FFProfile #{const FF_PROFILE_H264_MAIN}
pattern FFProfileH264Extended = FFProfile #{const FF_PROFILE_H264_EXTENDED}
pattern FFProfileH264High = FFProfile #{const FF_PROFILE_H264_HIGH}
pattern FFProfileH264High10 = FFProfile #{const FF_PROFILE_H264_HIGH_10}
pattern FFProfileH264High10Intra = FFProfile #{const FF_PROFILE_H264_HIGH_10_INTRA}
pattern FFProfileH264High422 = FFProfile #{const FF_PROFILE_H264_HIGH_422}
pattern FFProfileH264High422Intra = FFProfile #{const FF_PROFILE_H264_HIGH_422_INTRA}
pattern FFProfileH264High444 = FFProfile #{const FF_PROFILE_H264_HIGH_444}
pattern FFProfileH264High444Predictive = FFProfile #{const FF_PROFILE_H264_HIGH_444_PREDICTIVE}
pattern FFProfileH264High444Intra = FFProfile #{const FF_PROFILE_H264_HIGH_444_INTRA}
pattern FFProfileH264Cavlc444 = FFProfile #{const FF_PROFILE_H264_CAVLC_444}
pattern FFProfileVc1Simple = FFProfile #{const FF_PROFILE_VC1_SIMPLE}
pattern FFProfileVc1Main = FFProfile #{const FF_PROFILE_VC1_MAIN}
pattern FFProfileVc1Complex = FFProfile #{const FF_PROFILE_VC1_COMPLEX}
pattern FFProfileVc1Advanced = FFProfile #{const FF_PROFILE_VC1_ADVANCED}
pattern FFProfileMpeg4Simple = FFProfile #{const FF_PROFILE_MPEG4_SIMPLE}
pattern FFProfileMpeg4SimpleScalable = FFProfile #{const FF_PROFILE_MPEG4_SIMPLE_SCALABLE}
pattern FFProfileMpeg4Core = FFProfile #{const FF_PROFILE_MPEG4_CORE}
pattern FFProfileMpeg4Main = FFProfile #{const FF_PROFILE_MPEG4_MAIN}
pattern FFProfileMpeg4NBit = FFProfile #{const FF_PROFILE_MPEG4_N_BIT}
pattern FFProfileMpeg4ScalableTexture = FFProfile #{const FF_PROFILE_MPEG4_SCALABLE_TEXTURE}
pattern FFProfileMpeg4SimpleFaceAnimation = FFProfile #{const FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION}
pattern FFProfileMpeg4BasicAnimatedTexture = FFProfile #{const FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE}
pattern FFProfileMpeg4Hybrid = FFProfile #{const FF_PROFILE_MPEG4_HYBRID}
pattern FFProfileMpeg4AdvancedRealTime = FFProfile #{const FF_PROFILE_MPEG4_ADVANCED_REAL_TIME}
pattern FFProfileMpeg4CoreScalable = FFProfile #{const FF_PROFILE_MPEG4_CORE_SCALABLE}
pattern FFProfileMpeg4AdvancedCoding = FFProfile #{const FF_PROFILE_MPEG4_ADVANCED_CODING}
pattern FFProfileMpeg4AdvancedCore = FFProfile #{const FF_PROFILE_MPEG4_ADVANCED_CORE}
pattern FFProfileMpeg4AdvancedScalableTexture = FFProfile #{const FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE}
pattern FFProfileMpeg4SimpleStudio = FFProfile #{const FF_PROFILE_MPEG4_SIMPLE_STUDIO}
pattern FFProfileMpeg4AdvancedSimple = FFProfile #{const FF_PROFILE_MPEG4_ADVANCED_SIMPLE}
pattern FFProfileJpeg2000CstreamRestriction0 = FFProfile #{const FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_0}
pattern FFProfileJpeg2000CstreamRestriction1 = FFProfile #{const FF_PROFILE_JPEG2000_CSTREAM_RESTRICTION_1}
pattern FFProfileJpeg2000CstreamNoRestriction = FFProfile #{const FF_PROFILE_JPEG2000_CSTREAM_NO_RESTRICTION}
pattern FFProfileJpeg2000Dcinema2k = FFProfile #{const FF_PROFILE_JPEG2000_DCINEMA_2K}
pattern FFProfileJpeg2000Dcinema4k = FFProfile #{const FF_PROFILE_JPEG2000_DCINEMA_4K}
pattern FFProfileHevcMain = FFProfile #{const FF_PROFILE_HEVC_MAIN}
pattern FFProfileHevcMain10 = FFProfile #{const FF_PROFILE_HEVC_MAIN_10}
pattern FFProfileHevcMainStillPicture = FFProfile #{const FF_PROFILE_HEVC_MAIN_STILL_PICTURE}
pattern FFProfileHevcRext = FFProfile #{const FF_PROFILE_HEVC_REXT}

--ff_profile_dts_express = FF_PROFILE_DTS_EXPRESS,
--ff_profile_vp9_0 = FF_PROFILE_VP9_0,
--ff_profile_vp9_1 = FF_PROFILE_VP9_1,
--ff_profile_vp9_2 = FF_PROFILE_VP9_2,
--ff_profile_vp9_3 = FF_PROFILE_VP9_3,

-- | FF_BUG flags
newtype FFBug = FFBug CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern FFBugAutodetect = FFBug (#{const FF_BUG_AUTODETECT})
pattern FFBugOldMsmpeg4 = FFBug (#{const FF_BUG_OLD_MSMPEG4})
pattern FFBugXvidIlace = FFBug (#{const FF_BUG_XVID_ILACE})
pattern FFBugUmp4 = FFBug (#{const FF_BUG_UMP4})
pattern FFBugNoPadding = FFBug (#{const FF_BUG_NO_PADDING})
pattern FFBugAmv = FFBug (#{const FF_BUG_AMV})
pattern FFBugAcVlc = FFBug (#{const FF_BUG_AC_VLC})
pattern FFBugQpelChroma = FFBug (#{const FF_BUG_QPEL_CHROMA})
pattern FFBugStdQpel = FFBug (#{const FF_BUG_STD_QPEL})
pattern FFBugQpelChroma2 = FFBug (#{const FF_BUG_QPEL_CHROMA2})
pattern FFBugDirectBlocksize = FFBug (#{const FF_BUG_DIRECT_BLOCKSIZE})
pattern FFBugEdge = FFBug (#{const FF_BUG_EDGE})
pattern FFBugHpelChroma = FFBug (#{const FF_BUG_HPEL_CHROMA})
pattern FFBugDcClip = FFBug (#{const FF_BUG_DC_CLIP})
pattern FFBugMs = FFBug (#{const FF_BUG_MS})
pattern FFBugTruncated = FFBug (#{const FF_BUG_TRUNCATED})

-- | FFDCT flags
newtype FFDCT = FFDCT CInt deriving (Eq, Show, CEnum, Storable)
pattern FFDCTAuto = FFDCT (#{const FF_DCT_AUTO})
pattern FFDCTFastint = FFDCT (#{const FF_DCT_FASTINT})
pattern FFDCTInt = FFDCT (#{const FF_DCT_INT})
pattern FFDCTMmx = FFDCT (#{const FF_DCT_MMX})
pattern FFDCTAltivec = FFDCT (#{const FF_DCT_ALTIVEC})
pattern FFDCTFaan = FFDCT (#{const FF_DCT_FAAN})

-- | FFIdct flags
newtype FFIdct = FFIdct CInt deriving (Eq, Show, CEnum, Storable)
pattern FFIdctAuto = FFIdct (#{const FF_IDCT_AUTO})
pattern FFIdctInt = FFIdct (#{const FF_IDCT_INT})
pattern FFIdctSimple = FFIdct (#{const FF_IDCT_SIMPLE})
pattern FFIdctSimplemmx = FFIdct (#{const FF_IDCT_SIMPLEMMX})
pattern FFIdctArm = FFIdct (#{const FF_IDCT_ARM})
pattern FFIdctAltivec = FFIdct (#{const FF_IDCT_ALTIVEC})
pattern FFIdctSh4 = FFIdct (#{const FF_IDCT_SH4})
pattern FFIdctSimplearm = FFIdct (#{const FF_IDCT_SIMPLEARM})
pattern FFIdctIpp = FFIdct (#{const FF_IDCT_IPP})
pattern FFIdctXvid = FFIdct (#{const FF_IDCT_XVID})
pattern FFIdctXvidmmx = FFIdct (#{const FF_IDCT_XVIDMMX})
pattern FFIdctSimplearmv5te = FFIdct (#{const FF_IDCT_SIMPLEARMV5TE})
pattern FFIdctSimplearmv6 = FFIdct (#{const FF_IDCT_SIMPLEARMV6})
pattern FFIdctSimplevis = FFIdct (#{const FF_IDCT_SIMPLEVIS})
pattern FFIdctFaan = FFIdct (#{const FF_IDCT_FAAN})
pattern FFIdctSimpleneon = FFIdct (#{const FF_IDCT_SIMPLENEON})
pattern FFIdctSimplealpha = FFIdct (#{const FF_IDCT_SIMPLEALPHA})
pattern FFIdctSimpleauto = FFIdct (#{const FF_IDCT_SIMPLEAUTO})

-- | FF_EC_ constants
newtype FFEC = FFEC CInt deriving (Eq, Show, CEnum, Storable)
pattern FFECGuessMvs = FFEC (#{const FF_EC_GUESS_MVS})
pattern FFECDeblock = FFEC (#{const FF_EC_DEBLOCK})
pattern FFECFavorInter = FFEC (#{const FF_EC_FAVOR_INTER})

-- | FF_PRED_ constants
newtype FFPred = FFPred CInt deriving (Eq, Show, CEnum, Storable)
pattern FFPredLeft = FFPred (#{const FF_PRED_LEFT})
pattern FFPredPlane = FFPred (#{const FF_PRED_PLANE})
pattern FFPredMedian = FFPred (#{const FF_PRED_MEDIAN})

-- | FF_DEBUG_ flags
newtype FFDebug = FFDebug CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern FFDebugPictInfo = FFDebug (#{const FF_DEBUG_PICT_INFO})
pattern FFDebugRc = FFDebug (#{const FF_DEBUG_RC})
pattern FFDebugBitstream = FFDebug (#{const FF_DEBUG_BITSTREAM})
pattern FFDebugMbType = FFDebug (#{const FF_DEBUG_MB_TYPE})
pattern FFDebugQp = FFDebug (#{const FF_DEBUG_QP})
pattern FFDebugMv = FFDebug (#{const FF_DEBUG_MV})
pattern FFDebugDctCoeff = FFDebug (#{const FF_DEBUG_DCT_COEFF})
pattern FFDebugSkip = FFDebug (#{const FF_DEBUG_SKIP})
pattern FFDebugStartcode = FFDebug (#{const FF_DEBUG_STARTCODE})
pattern FFDebugPts = FFDebug (#{const FF_DEBUG_PTS})
pattern FFDebugEr = FFDebug (#{const FF_DEBUG_ER})
pattern FFDebugMmco = FFDebug (#{const FF_DEBUG_MMCO})
pattern FFDebugBugs = FFDebug (#{const FF_DEBUG_BUGS})
pattern FFDebugVisQp = FFDebug (#{const FF_DEBUG_VIS_QP})
pattern FFDebugVisMbType = FFDebug (#{const FF_DEBUG_VIS_MB_TYPE})
pattern FFDebugBuffers = FFDebug (#{const FF_DEBUG_BUFFERS})
pattern FFDebugThreads = FFDebug (#{const FF_DEBUG_THREADS})
--pattern FFDebugGreenMd = FFDebug (#{const FF_DEBUG_GREEN_MD})
pattern FFDebugNomc = FFDebug (#{const FF_DEBUG_NOMC})
pattern FFDebugVisMvPFor = FFDebug (#{const FF_DEBUG_VIS_MV_P_FOR})
pattern FFDebugVisMvBFor = FFDebug (#{const FF_DEBUG_VIS_MV_B_FOR})
pattern FFDebugVisMvBBack = FFDebug (#{const FF_DEBUG_VIS_MV_B_BACK})

-- | FF_CODER_TYPE constants
newtype FFCoderType = FFCoderType CInt deriving (Eq, Show, CEnum, Storable)
pattern FFCoderTypeVlc = FFCoderType (#{const FF_CODER_TYPE_VLC})
pattern FFCoderTypeAc = FFCoderType (#{const FF_CODER_TYPE_AC})
pattern FFCoderTypeRaw = FFCoderType (#{const FF_CODER_TYPE_RAW})
pattern FFCoderTypeRle = FFCoderType (#{const FF_CODER_TYPE_RLE})
pattern FFCoderTypeDeflate = FFCoderType (#{const FF_CODER_TYPE_DEFLATE})

-- | FF_MB_DECISION constants
newtype FFMBDecision = FFMBDecision CInt deriving (Eq, Show, CEnum, Storable)
pattern FFMBDecisionSimple = FFMBDecision (#{const FF_MB_DECISION_SIMPLE})
pattern FFMBDecisionBits = FFMBDecision (#{const FF_MB_DECISION_BITS})
pattern FFMBDecisionRd = FFMBDecision (#{const FF_MB_DECISION_RD})

-- | FF_LEVEL_ constants
newtype FFLevel = FFLevel CInt deriving (Eq, Show, CEnum, Storable)
pattern FFLevelUnknown = FFLevel (#{const FF_LEVEL_UNKNOWN})

-- | FF_SUB_CHARENC_MODE_ constants
newtype FFSubCharencMode = FFSubCharencMode CInt deriving (Eq, Show, CEnum, Storable)
pattern FFSubCharencModeDoNothing = FFSubCharencMode (#{const FF_SUB_CHARENC_MODE_DO_NOTHING})
pattern FFSubCharencModeAutomatic = FFSubCharencMode (#{const FF_SUB_CHARENC_MODE_AUTOMATIC})
pattern FFSubCharencModePreDecoder = FFSubCharencMode (#{const FF_SUB_CHARENC_MODE_PRE_DECODER})

-- | FF_THREAD_ constants
newtype FFThread = FFThread CInt deriving (Eq, Show, CEnum, Storable)
pattern FFThreadFrame = FFThread (#{const FF_THREAD_FRAME})
pattern FFThreadSlice = FFThread (#{const FF_THREAD_SLICE})

-- | FF_CMP_ constants
newtype FFCmp = FFCmp CInt deriving (Eq, Show, CEnum, Storable)
pattern FFCmpSad = FFCmp (#{const FF_CMP_SAD})
pattern FFCmpSse = FFCmp (#{const FF_CMP_SSE})
pattern FFCmpSatd = FFCmp (#{const FF_CMP_SATD})
pattern FFCmpDct = FFCmp (#{const FF_CMP_DCT})
pattern FFCmpPsnr = FFCmp (#{const FF_CMP_PSNR})
pattern FFCmpBit = FFCmp (#{const FF_CMP_BIT})
pattern FFCmpRd = FFCmp (#{const FF_CMP_RD})
pattern FFCmpZero = FFCmp (#{const FF_CMP_ZERO})
pattern FFCmpVsad = FFCmp (#{const FF_CMP_VSAD})
pattern FFCmpVsse = FFCmp (#{const FF_CMP_VSSE})
pattern FFCmpNsse = FFCmp (#{const FF_CMP_NSSE})
pattern FFCmpW53 = FFCmp (#{const FF_CMP_W53})
pattern FFCmpW97 = FFCmp (#{const FF_CMP_W97})
pattern FFCmpDctmax = FFCmp (#{const FF_CMP_DCTMAX})
pattern FFCmpDct264 = FFCmp (#{const FF_CMP_DCT264})
pattern FFCmpChroma = FFCmp (#{const FF_CMP_CHROMA})

-- | SLICE_FLAG_ flags
newtype SliceFlags = SliceFlags CInt deriving (Eq, Show, CEnum, CFlags, Storable)
pattern SliceFlagCodedOrder = SliceFlags (#{const SLICE_FLAG_CODED_ORDER})
pattern SliceFlagAllowField = SliceFlags (#{const SLICE_FLAG_ALLOW_FIELD})
pattern SliceFlagAllowPlane = SliceFlags (#{const SLICE_FLAG_ALLOW_PLANE})

-- | AVCodecId
newtype AVCodecID = AVCodecID CInt deriving (Eq, Show, CEnum, Storable)
pattern AVCodecIdNone = AVCodecID #{const AV_CODEC_ID_NONE}
pattern AVCodecIdMpeg1video = AVCodecID #{const AV_CODEC_ID_MPEG1VIDEO}
pattern AVCodecIdMpeg2video = AVCodecID #{const AV_CODEC_ID_MPEG2VIDEO}
pattern AVCodecIdH261 = AVCodecID #{const AV_CODEC_ID_H261}
pattern AVCodecIdH263 = AVCodecID #{const AV_CODEC_ID_H263}
pattern AVCodecIdRv10 = AVCodecID #{const AV_CODEC_ID_RV10}
pattern AVCodecIdRv20 = AVCodecID #{const AV_CODEC_ID_RV20}
pattern AVCodecIdMjpeg = AVCodecID #{const AV_CODEC_ID_MJPEG}
pattern AVCodecIdMjpegb = AVCodecID #{const AV_CODEC_ID_MJPEGB}
pattern AVCodecIdLjpeg = AVCodecID #{const AV_CODEC_ID_LJPEG}
pattern AVCodecIdSp5x = AVCodecID #{const AV_CODEC_ID_SP5X}
pattern AVCodecIdJpegls = AVCodecID #{const AV_CODEC_ID_JPEGLS}
pattern AVCodecIdMpeg4 = AVCodecID #{const AV_CODEC_ID_MPEG4}
pattern AVCodecIdRawvideo = AVCodecID #{const AV_CODEC_ID_RAWVIDEO}
pattern AVCodecIdMsmpeg4v1 = AVCodecID #{const AV_CODEC_ID_MSMPEG4V1}
pattern AVCodecIdMsmpeg4v2 = AVCodecID #{const AV_CODEC_ID_MSMPEG4V2}
pattern AVCodecIdMsmpeg4v3 = AVCodecID #{const AV_CODEC_ID_MSMPEG4V3}
pattern AVCodecIdWmv1 = AVCodecID #{const AV_CODEC_ID_WMV1}
pattern AVCodecIdWmv2 = AVCodecID #{const AV_CODEC_ID_WMV2}
pattern AVCodecIdH263p = AVCodecID #{const AV_CODEC_ID_H263P}
pattern AVCodecIdH263i = AVCodecID #{const AV_CODEC_ID_H263I}
pattern AVCodecIdFlv1 = AVCodecID #{const AV_CODEC_ID_FLV1}
pattern AVCodecIdSvq1 = AVCodecID #{const AV_CODEC_ID_SVQ1}
pattern AVCodecIdSvq3 = AVCodecID #{const AV_CODEC_ID_SVQ3}
pattern AVCodecIdDvvideo = AVCodecID #{const AV_CODEC_ID_DVVIDEO}
pattern AVCodecIdHuffyuv = AVCodecID #{const AV_CODEC_ID_HUFFYUV}
pattern AVCodecIdCyuv = AVCodecID #{const AV_CODEC_ID_CYUV}
pattern AVCodecIdH264 = AVCodecID #{const AV_CODEC_ID_H264}
pattern AVCodecIdIndeo3 = AVCodecID #{const AV_CODEC_ID_INDEO3}
pattern AVCodecIdVp3 = AVCodecID #{const AV_CODEC_ID_VP3}
pattern AVCodecIdTheora = AVCodecID #{const AV_CODEC_ID_THEORA}
pattern AVCodecIdAsv1 = AVCodecID #{const AV_CODEC_ID_ASV1}
pattern AVCodecIdAsv2 = AVCodecID #{const AV_CODEC_ID_ASV2}
pattern AVCodecIdFfv1 = AVCodecID #{const AV_CODEC_ID_FFV1}
pattern AVCodecId4xm = AVCodecID #{const AV_CODEC_ID_4XM}
pattern AVCodecIdVcr1 = AVCodecID #{const AV_CODEC_ID_VCR1}
pattern AVCodecIdCljr = AVCodecID #{const AV_CODEC_ID_CLJR}
pattern AVCodecIdMdec = AVCodecID #{const AV_CODEC_ID_MDEC}
pattern AVCodecIdRoq = AVCodecID #{const AV_CODEC_ID_ROQ}
pattern AVCodecIdInterplayVideo = AVCodecID #{const AV_CODEC_ID_INTERPLAY_VIDEO}
pattern AVCodecIdXanWc3 = AVCodecID #{const AV_CODEC_ID_XAN_WC3}
pattern AVCodecIdXanWc4 = AVCodecID #{const AV_CODEC_ID_XAN_WC4}
pattern AVCodecIdRpza = AVCodecID #{const AV_CODEC_ID_RPZA}
pattern AVCodecIdCinepak = AVCodecID #{const AV_CODEC_ID_CINEPAK}
pattern AVCodecIdWsVqa = AVCodecID #{const AV_CODEC_ID_WS_VQA}
pattern AVCodecIdMsrle = AVCodecID #{const AV_CODEC_ID_MSRLE}
pattern AVCodecIdMsvideo1 = AVCodecID #{const AV_CODEC_ID_MSVIDEO1}
pattern AVCodecIdIdcin = AVCodecID #{const AV_CODEC_ID_IDCIN}
pattern AVCodecId8bps = AVCodecID #{const AV_CODEC_ID_8BPS}
pattern AVCodecIdSmc = AVCodecID #{const AV_CODEC_ID_SMC}
pattern AVCodecIdFlic = AVCodecID #{const AV_CODEC_ID_FLIC}
pattern AVCodecIdTruemotion1 = AVCodecID #{const AV_CODEC_ID_TRUEMOTION1}
pattern AVCodecIdVmdvideo = AVCodecID #{const AV_CODEC_ID_VMDVIDEO}
pattern AVCodecIdMszh = AVCodecID #{const AV_CODEC_ID_MSZH}
pattern AVCodecIdZlib = AVCodecID #{const AV_CODEC_ID_ZLIB}
pattern AVCodecIdQtrle = AVCodecID #{const AV_CODEC_ID_QTRLE}
pattern AVCodecIdTscc = AVCodecID #{const AV_CODEC_ID_TSCC}
pattern AVCodecIdUlti = AVCodecID #{const AV_CODEC_ID_ULTI}
pattern AVCodecIdQdraw = AVCodecID #{const AV_CODEC_ID_QDRAW}
pattern AVCodecIdVixl = AVCodecID #{const AV_CODEC_ID_VIXL}
pattern AVCodecIdQpeg = AVCodecID #{const AV_CODEC_ID_QPEG}
pattern AVCodecIdPng = AVCodecID #{const AV_CODEC_ID_PNG}
pattern AVCodecIdPpm = AVCodecID #{const AV_CODEC_ID_PPM}
pattern AVCodecIdPbm = AVCodecID #{const AV_CODEC_ID_PBM}
pattern AVCodecIdPgm = AVCodecID #{const AV_CODEC_ID_PGM}
pattern AVCodecIdPgmyuv = AVCodecID #{const AV_CODEC_ID_PGMYUV}
pattern AVCodecIdPam = AVCodecID #{const AV_CODEC_ID_PAM}
pattern AVCodecIdFfvhuff = AVCodecID #{const AV_CODEC_ID_FFVHUFF}
pattern AVCodecIdRv30 = AVCodecID #{const AV_CODEC_ID_RV30}
pattern AVCodecIdRv40 = AVCodecID #{const AV_CODEC_ID_RV40}
pattern AVCodecIdVc1 = AVCodecID #{const AV_CODEC_ID_VC1}
pattern AVCodecIdWmv3 = AVCodecID #{const AV_CODEC_ID_WMV3}
pattern AVCodecIdLoco = AVCodecID #{const AV_CODEC_ID_LOCO}
pattern AVCodecIdWnv1 = AVCodecID #{const AV_CODEC_ID_WNV1}
pattern AVCodecIdAasc = AVCodecID #{const AV_CODEC_ID_AASC}
pattern AVCodecIdIndeo2 = AVCodecID #{const AV_CODEC_ID_INDEO2}
pattern AVCodecIdFraps = AVCodecID #{const AV_CODEC_ID_FRAPS}
pattern AVCodecIdTruemotion2 = AVCodecID #{const AV_CODEC_ID_TRUEMOTION2}
pattern AVCodecIdBmp = AVCodecID #{const AV_CODEC_ID_BMP}
pattern AVCodecIdCscd = AVCodecID #{const AV_CODEC_ID_CSCD}
pattern AVCodecIdMmvideo = AVCodecID #{const AV_CODEC_ID_MMVIDEO}
pattern AVCodecIdZmbv = AVCodecID #{const AV_CODEC_ID_ZMBV}
pattern AVCodecIdAvs = AVCodecID #{const AV_CODEC_ID_AVS}
pattern AVCodecIdSmackvideo = AVCodecID #{const AV_CODEC_ID_SMACKVIDEO}
pattern AVCodecIdNuv = AVCodecID #{const AV_CODEC_ID_NUV}
pattern AVCodecIdKmvc = AVCodecID #{const AV_CODEC_ID_KMVC}
pattern AVCodecIdFlashsv = AVCodecID #{const AV_CODEC_ID_FLASHSV}
pattern AVCodecIdCavs = AVCodecID #{const AV_CODEC_ID_CAVS}
pattern AVCodecIdJpeg2000 = AVCodecID #{const AV_CODEC_ID_JPEG2000}
pattern AVCodecIdVmnc = AVCodecID #{const AV_CODEC_ID_VMNC}
pattern AVCodecIdVp5 = AVCodecID #{const AV_CODEC_ID_VP5}
pattern AVCodecIdVp6 = AVCodecID #{const AV_CODEC_ID_VP6}
pattern AVCodecIdVp6f = AVCodecID #{const AV_CODEC_ID_VP6F}
pattern AVCodecIdTarga = AVCodecID #{const AV_CODEC_ID_TARGA}
pattern AVCodecIdDsicinvideo = AVCodecID #{const AV_CODEC_ID_DSICINVIDEO}
pattern AVCodecIdTiertexseqvideo = AVCodecID #{const AV_CODEC_ID_TIERTEXSEQVIDEO}
pattern AVCodecIdTiff = AVCodecID #{const AV_CODEC_ID_TIFF}
pattern AVCodecIdGif = AVCodecID #{const AV_CODEC_ID_GIF}
pattern AVCodecIdDxa = AVCodecID #{const AV_CODEC_ID_DXA}
pattern AVCodecIdDnxhd = AVCodecID #{const AV_CODEC_ID_DNXHD}
pattern AVCodecIdThp = AVCodecID #{const AV_CODEC_ID_THP}
pattern AVCodecIdSgi = AVCodecID #{const AV_CODEC_ID_SGI}
pattern AVCodecIdC93 = AVCodecID #{const AV_CODEC_ID_C93}
pattern AVCodecIdBethsoftvid = AVCodecID #{const AV_CODEC_ID_BETHSOFTVID}
pattern AVCodecIdPtx = AVCodecID #{const AV_CODEC_ID_PTX}
pattern AVCodecIdTxd = AVCodecID #{const AV_CODEC_ID_TXD}
pattern AVCodecIdVp6a = AVCodecID #{const AV_CODEC_ID_VP6A}
pattern AVCodecIdAmv = AVCodecID #{const AV_CODEC_ID_AMV}
pattern AVCodecIdVb = AVCodecID #{const AV_CODEC_ID_VB}
pattern AVCodecIdPcx = AVCodecID #{const AV_CODEC_ID_PCX}
pattern AVCodecIdSunrast = AVCodecID #{const AV_CODEC_ID_SUNRAST}
pattern AVCodecIdIndeo4 = AVCodecID #{const AV_CODEC_ID_INDEO4}
pattern AVCodecIdIndeo5 = AVCodecID #{const AV_CODEC_ID_INDEO5}
pattern AVCodecIdMimic = AVCodecID #{const AV_CODEC_ID_MIMIC}
pattern AVCodecIdRl2 = AVCodecID #{const AV_CODEC_ID_RL2}
pattern AVCodecIdEscape124 = AVCodecID #{const AV_CODEC_ID_ESCAPE124}
pattern AVCodecIdDirac = AVCodecID #{const AV_CODEC_ID_DIRAC}
pattern AVCodecIdBfi = AVCodecID #{const AV_CODEC_ID_BFI}
pattern AVCodecIdCmv = AVCodecID #{const AV_CODEC_ID_CMV}
pattern AVCodecIdMotionpixels = AVCodecID #{const AV_CODEC_ID_MOTIONPIXELS}
pattern AVCodecIdTgv = AVCodecID #{const AV_CODEC_ID_TGV}
pattern AVCodecIdTgq = AVCodecID #{const AV_CODEC_ID_TGQ}
pattern AVCodecIdTqi = AVCodecID #{const AV_CODEC_ID_TQI}
pattern AVCodecIdAura = AVCodecID #{const AV_CODEC_ID_AURA}
pattern AVCodecIdAura2 = AVCodecID #{const AV_CODEC_ID_AURA2}
pattern AVCodecIdV210x = AVCodecID #{const AV_CODEC_ID_V210X}
pattern AVCodecIdTmv = AVCodecID #{const AV_CODEC_ID_TMV}
pattern AVCodecIdV210 = AVCodecID #{const AV_CODEC_ID_V210}
pattern AVCodecIdDpx = AVCodecID #{const AV_CODEC_ID_DPX}
pattern AVCodecIdMad = AVCodecID #{const AV_CODEC_ID_MAD}
pattern AVCodecIdFrwu = AVCodecID #{const AV_CODEC_ID_FRWU}
pattern AVCodecIdFlashsv2 = AVCodecID #{const AV_CODEC_ID_FLASHSV2}
pattern AVCodecIdCdgraphics = AVCodecID #{const AV_CODEC_ID_CDGRAPHICS}
pattern AVCodecIdR210 = AVCodecID #{const AV_CODEC_ID_R210}
pattern AVCodecIdAnm = AVCodecID #{const AV_CODEC_ID_ANM}
pattern AVCodecIdBinkvideo = AVCodecID #{const AV_CODEC_ID_BINKVIDEO}
pattern AVCodecIdIffIlbm = AVCodecID #{const AV_CODEC_ID_IFF_ILBM}
pattern AVCodecIdIffByterun1 = AVCodecID #{const AV_CODEC_ID_IFF_BYTERUN1}
pattern AVCodecIdKgv1 = AVCodecID #{const AV_CODEC_ID_KGV1}
pattern AVCodecIdYop = AVCodecID #{const AV_CODEC_ID_YOP}
pattern AVCodecIdVp8 = AVCodecID #{const AV_CODEC_ID_VP8}
pattern AVCodecIdPictor = AVCodecID #{const AV_CODEC_ID_PICTOR}
pattern AVCodecIdAnsi = AVCodecID #{const AV_CODEC_ID_ANSI}
pattern AVCodecIdA64Multi = AVCodecID #{const AV_CODEC_ID_A64_MULTI}
pattern AVCodecIdA64Multi5 = AVCodecID #{const AV_CODEC_ID_A64_MULTI5}
pattern AVCodecIdR10k = AVCodecID #{const AV_CODEC_ID_R10K}
pattern AVCodecIdMxpeg = AVCodecID #{const AV_CODEC_ID_MXPEG}
pattern AVCodecIdLagarith = AVCodecID #{const AV_CODEC_ID_LAGARITH}
pattern AVCodecIdProres = AVCodecID #{const AV_CODEC_ID_PRORES}
pattern AVCodecIdJv = AVCodecID #{const AV_CODEC_ID_JV}
pattern AVCodecIdDfa = AVCodecID #{const AV_CODEC_ID_DFA}
pattern AVCodecIdWmv3image = AVCodecID #{const AV_CODEC_ID_WMV3IMAGE}
pattern AVCodecIdVc1image = AVCodecID #{const AV_CODEC_ID_VC1IMAGE}
pattern AVCodecIdUtvideo = AVCodecID #{const AV_CODEC_ID_UTVIDEO}
pattern AVCodecIdBmvVideo = AVCodecID #{const AV_CODEC_ID_BMV_VIDEO}
pattern AVCodecIdVble = AVCodecID #{const AV_CODEC_ID_VBLE}
pattern AVCodecIdDxtory = AVCodecID #{const AV_CODEC_ID_DXTORY}
pattern AVCodecIdV410 = AVCodecID #{const AV_CODEC_ID_V410}
pattern AVCodecIdXwd = AVCodecID #{const AV_CODEC_ID_XWD}
pattern AVCodecIdCdxl = AVCodecID #{const AV_CODEC_ID_CDXL}
pattern AVCodecIdXbm = AVCodecID #{const AV_CODEC_ID_XBM}
pattern AVCodecIdZerocodec = AVCodecID #{const AV_CODEC_ID_ZEROCODEC}
pattern AVCodecIdMss1 = AVCodecID #{const AV_CODEC_ID_MSS1}
pattern AVCodecIdMsa1 = AVCodecID #{const AV_CODEC_ID_MSA1}
pattern AVCodecIdTscc2 = AVCodecID #{const AV_CODEC_ID_TSCC2}
pattern AVCodecIdMts2 = AVCodecID #{const AV_CODEC_ID_MTS2}
pattern AVCodecIdCllc = AVCodecID #{const AV_CODEC_ID_CLLC}
pattern AVCodecIdMss2 = AVCodecID #{const AV_CODEC_ID_MSS2}
pattern AVCodecIdVp9 = AVCodecID #{const AV_CODEC_ID_VP9}
pattern AVCodecIdAic = AVCodecID #{const AV_CODEC_ID_AIC}
pattern AVCodecIdEscape130Deprecated = AVCodecID #{const AV_CODEC_ID_ESCAPE130_DEPRECATED}
pattern AVCodecIdG2mDeprecated = AVCodecID #{const AV_CODEC_ID_G2M_DEPRECATED}
pattern AVCodecIdWebpDeprecated = AVCodecID #{const AV_CODEC_ID_WEBP_DEPRECATED}
pattern AVCodecIdHnm4Video = AVCodecID #{const AV_CODEC_ID_HNM4_VIDEO}
pattern AVCodecIdHevcDeprecated = AVCodecID #{const AV_CODEC_ID_HEVC_DEPRECATED}
pattern AVCodecIdFic = AVCodecID #{const AV_CODEC_ID_FIC}
pattern AVCodecIdAliasPix = AVCodecID #{const AV_CODEC_ID_ALIAS_PIX}
pattern AVCodecIdBrenderPixDeprecated = AVCodecID #{const AV_CODEC_ID_BRENDER_PIX_DEPRECATED}
pattern AVCodecIdPafVideoDeprecated = AVCodecID #{const AV_CODEC_ID_PAF_VIDEO_DEPRECATED}
pattern AVCodecIdExrDeprecated = AVCodecID #{const AV_CODEC_ID_EXR_DEPRECATED}
pattern AVCodecIdVp7Deprecated = AVCodecID #{const AV_CODEC_ID_VP7_DEPRECATED}
pattern AVCodecIdSanmDeprecated = AVCodecID #{const AV_CODEC_ID_SANM_DEPRECATED}
pattern AVCodecIdSgirleDeprecated = AVCodecID #{const AV_CODEC_ID_SGIRLE_DEPRECATED}
pattern AVCodecIdMvc1Deprecated = AVCodecID #{const AV_CODEC_ID_MVC1_DEPRECATED}
pattern AVCodecIdMvc2Deprecated = AVCodecID #{const AV_CODEC_ID_MVC2_DEPRECATED}
pattern AVCodecIdBrenderPix = AVCodecID #{const AV_CODEC_ID_BRENDER_PIX}
pattern AVCodecIdY41p = AVCodecID #{const AV_CODEC_ID_Y41P}
pattern AVCodecIdEscape130 = AVCodecID #{const AV_CODEC_ID_ESCAPE130}
pattern AVCodecIdExr = AVCodecID #{const AV_CODEC_ID_EXR}
pattern AVCodecIdAvrp = AVCodecID #{const AV_CODEC_ID_AVRP}
pattern AVCodecId012v = AVCodecID #{const AV_CODEC_ID_012V}
pattern AVCodecIdG2m = AVCodecID #{const AV_CODEC_ID_G2M}
pattern AVCodecIdAvui = AVCodecID #{const AV_CODEC_ID_AVUI}
pattern AVCodecIdAyuv = AVCodecID #{const AV_CODEC_ID_AYUV}
pattern AVCodecIdTargaY216 = AVCodecID #{const AV_CODEC_ID_TARGA_Y216}
pattern AVCodecIdV308 = AVCodecID #{const AV_CODEC_ID_V308}
pattern AVCodecIdV408 = AVCodecID #{const AV_CODEC_ID_V408}
pattern AVCodecIdYuv4 = AVCodecID #{const AV_CODEC_ID_YUV4}
pattern AVCodecIdSanm = AVCodecID #{const AV_CODEC_ID_SANM}
pattern AVCodecIdPafVideo = AVCodecID #{const AV_CODEC_ID_PAF_VIDEO}
pattern AVCodecIdAvrn = AVCodecID #{const AV_CODEC_ID_AVRN}
pattern AVCodecIdCpia = AVCodecID #{const AV_CODEC_ID_CPIA}
pattern AVCodecIdXface = AVCodecID #{const AV_CODEC_ID_XFACE}
pattern AVCodecIdSgirle = AVCodecID #{const AV_CODEC_ID_SGIRLE}
pattern AVCodecIdMvc1 = AVCodecID #{const AV_CODEC_ID_MVC1}
pattern AVCodecIdMvc2 = AVCodecID #{const AV_CODEC_ID_MVC2}
pattern AVCodecIdSnow = AVCodecID #{const AV_CODEC_ID_SNOW}
pattern AVCodecIdWebp = AVCodecID #{const AV_CODEC_ID_WEBP}
pattern AVCodecIdSmvjpeg = AVCodecID #{const AV_CODEC_ID_SMVJPEG}
pattern AVCodecIdHevc = AVCodecID #{const AV_CODEC_ID_HEVC}
pattern AVCodecIdH265 = AVCodecID #{const AV_CODEC_ID_H265}
pattern AVCodecIdVp7 = AVCodecID #{const AV_CODEC_ID_VP7}
pattern AVCodecIdApng = AVCodecID #{const AV_CODEC_ID_APNG}
pattern AVCodecIdFirstAudio = AVCodecID #{const AV_CODEC_ID_FIRST_AUDIO}
pattern AVCodecIdPcmS16le = AVCodecID #{const AV_CODEC_ID_PCM_S16LE}
pattern AVCodecIdPcmS16be = AVCodecID #{const AV_CODEC_ID_PCM_S16BE}
pattern AVCodecIdPcmU16le = AVCodecID #{const AV_CODEC_ID_PCM_U16LE}
pattern AVCodecIdPcmU16be = AVCodecID #{const AV_CODEC_ID_PCM_U16BE}
pattern AVCodecIdPcmS8 = AVCodecID #{const AV_CODEC_ID_PCM_S8}
pattern AVCodecIdPcmU8 = AVCodecID #{const AV_CODEC_ID_PCM_U8}
pattern AVCodecIdPcmMulaw = AVCodecID #{const AV_CODEC_ID_PCM_MULAW}
pattern AVCodecIdPcmAlaw = AVCodecID #{const AV_CODEC_ID_PCM_ALAW}
pattern AVCodecIdPcmS32le = AVCodecID #{const AV_CODEC_ID_PCM_S32LE}
pattern AVCodecIdPcmS32be = AVCodecID #{const AV_CODEC_ID_PCM_S32BE}
pattern AVCodecIdPcmU32le = AVCodecID #{const AV_CODEC_ID_PCM_U32LE}
pattern AVCodecIdPcmU32be = AVCodecID #{const AV_CODEC_ID_PCM_U32BE}
pattern AVCodecIdPcmS24le = AVCodecID #{const AV_CODEC_ID_PCM_S24LE}
pattern AVCodecIdPcmS24be = AVCodecID #{const AV_CODEC_ID_PCM_S24BE}
pattern AVCodecIdPcmU24le = AVCodecID #{const AV_CODEC_ID_PCM_U24LE}
pattern AVCodecIdPcmU24be = AVCodecID #{const AV_CODEC_ID_PCM_U24BE}
pattern AVCodecIdPcmS24daud = AVCodecID #{const AV_CODEC_ID_PCM_S24DAUD}
pattern AVCodecIdPcmZork = AVCodecID #{const AV_CODEC_ID_PCM_ZORK}
pattern AVCodecIdPcmS16lePlanar = AVCodecID #{const AV_CODEC_ID_PCM_S16LE_PLANAR}
pattern AVCodecIdPcmDvd = AVCodecID #{const AV_CODEC_ID_PCM_DVD}
pattern AVCodecIdPcmF32be = AVCodecID #{const AV_CODEC_ID_PCM_F32BE}
pattern AVCodecIdPcmF32le = AVCodecID #{const AV_CODEC_ID_PCM_F32LE}
pattern AVCodecIdPcmF64be = AVCodecID #{const AV_CODEC_ID_PCM_F64BE}
pattern AVCodecIdPcmF64le = AVCodecID #{const AV_CODEC_ID_PCM_F64LE}
pattern AVCodecIdPcmBluray = AVCodecID #{const AV_CODEC_ID_PCM_BLURAY}
pattern AVCodecIdPcmLxf = AVCodecID #{const AV_CODEC_ID_PCM_LXF}
pattern AVCodecIdS302m = AVCodecID #{const AV_CODEC_ID_S302M}
pattern AVCodecIdPcmS8Planar = AVCodecID #{const AV_CODEC_ID_PCM_S8_PLANAR}
pattern AVCodecIdPcmS24lePlanarDeprecated = AVCodecID #{const AV_CODEC_ID_PCM_S24LE_PLANAR_DEPRECATED}
pattern AVCodecIdPcmS32lePlanarDeprecated = AVCodecID #{const AV_CODEC_ID_PCM_S32LE_PLANAR_DEPRECATED}
pattern AVCodecIdPcmS24lePlanar = AVCodecID #{const AV_CODEC_ID_PCM_S24LE_PLANAR}
pattern AVCodecIdPcmS32lePlanar = AVCodecID #{const AV_CODEC_ID_PCM_S32LE_PLANAR}
pattern AVCodecIdPcmS16bePlanar = AVCodecID #{const AV_CODEC_ID_PCM_S16BE_PLANAR}
pattern AVCodecIdAdpcmImaQt = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_QT}
pattern AVCodecIdAdpcmImaWav = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_WAV}
pattern AVCodecIdAdpcmImaDk3 = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_DK3}
pattern AVCodecIdAdpcmImaDk4 = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_DK4}
pattern AVCodecIdAdpcmImaWs = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_WS}
pattern AVCodecIdAdpcmImaSmjpeg = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_SMJPEG}
pattern AVCodecIdAdpcmMs = AVCodecID #{const AV_CODEC_ID_ADPCM_MS}
pattern AVCodecIdAdpcm4xm = AVCodecID #{const AV_CODEC_ID_ADPCM_4XM}
pattern AVCodecIdAdpcmXa = AVCodecID #{const AV_CODEC_ID_ADPCM_XA}
pattern AVCodecIdAdpcmAdx = AVCodecID #{const AV_CODEC_ID_ADPCM_ADX}
pattern AVCodecIdAdpcmEa = AVCodecID #{const AV_CODEC_ID_ADPCM_EA}
pattern AVCodecIdAdpcmG726 = AVCodecID #{const AV_CODEC_ID_ADPCM_G726}
pattern AVCodecIdAdpcmCt = AVCodecID #{const AV_CODEC_ID_ADPCM_CT}
pattern AVCodecIdAdpcmSwf = AVCodecID #{const AV_CODEC_ID_ADPCM_SWF}
pattern AVCodecIdAdpcmYamaha = AVCodecID #{const AV_CODEC_ID_ADPCM_YAMAHA}
pattern AVCodecIdAdpcmSbpro4 = AVCodecID #{const AV_CODEC_ID_ADPCM_SBPRO_4}
pattern AVCodecIdAdpcmSbpro3 = AVCodecID #{const AV_CODEC_ID_ADPCM_SBPRO_3}
pattern AVCodecIdAdpcmSbpro2 = AVCodecID #{const AV_CODEC_ID_ADPCM_SBPRO_2}
pattern AVCodecIdAdpcmThp = AVCodecID #{const AV_CODEC_ID_ADPCM_THP}
pattern AVCodecIdAdpcmImaAmv = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_AMV}
pattern AVCodecIdAdpcmEaR1 = AVCodecID #{const AV_CODEC_ID_ADPCM_EA_R1}
pattern AVCodecIdAdpcmEaR3 = AVCodecID #{const AV_CODEC_ID_ADPCM_EA_R3}
pattern AVCodecIdAdpcmEaR2 = AVCodecID #{const AV_CODEC_ID_ADPCM_EA_R2}
pattern AVCodecIdAdpcmImaEaSead = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_EA_SEAD}
pattern AVCodecIdAdpcmImaEaEacs = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_EA_EACS}
pattern AVCodecIdAdpcmEaXas = AVCodecID #{const AV_CODEC_ID_ADPCM_EA_XAS}
pattern AVCodecIdAdpcmEaMaxisXa = AVCodecID #{const AV_CODEC_ID_ADPCM_EA_MAXIS_XA}
pattern AVCodecIdAdpcmImaIss = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_ISS}
pattern AVCodecIdAdpcmG722 = AVCodecID #{const AV_CODEC_ID_ADPCM_G722}
pattern AVCodecIdAdpcmImaApc = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_APC}
pattern AVCodecIdAdpcmVimaDeprecated = AVCodecID #{const AV_CODEC_ID_ADPCM_VIMA_DEPRECATED}
pattern AVCodecIdAdpcmVima = AVCodecID #{const AV_CODEC_ID_ADPCM_VIMA}
pattern AVCodecIdAdpcmAfc = AVCodecID #{const AV_CODEC_ID_ADPCM_AFC}
pattern AVCodecIdAdpcmImaOki = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_OKI}
pattern AVCodecIdAdpcmDtk = AVCodecID #{const AV_CODEC_ID_ADPCM_DTK}
pattern AVCodecIdAdpcmImaRad = AVCodecID #{const AV_CODEC_ID_ADPCM_IMA_RAD}
pattern AVCodecIdAdpcmG726le = AVCodecID #{const AV_CODEC_ID_ADPCM_G726LE}
pattern AVCodecIdAmrNb = AVCodecID #{const AV_CODEC_ID_AMR_NB}
pattern AVCodecIdAmrWb = AVCodecID #{const AV_CODEC_ID_AMR_WB}
pattern AVCodecIdRa144 = AVCodecID #{const AV_CODEC_ID_RA_144}
pattern AVCodecIdRa288 = AVCodecID #{const AV_CODEC_ID_RA_288}
pattern AVCodecIdRoqDpcm = AVCodecID #{const AV_CODEC_ID_ROQ_DPCM}
pattern AVCodecIdInterplayDpcm = AVCodecID #{const AV_CODEC_ID_INTERPLAY_DPCM}
pattern AVCodecIdXanDpcm = AVCodecID #{const AV_CODEC_ID_XAN_DPCM}
pattern AVCodecIdSolDpcm = AVCodecID #{const AV_CODEC_ID_SOL_DPCM}
pattern AVCodecIdMp2 = AVCodecID #{const AV_CODEC_ID_MP2}
pattern AVCodecIdMp3 = AVCodecID #{const AV_CODEC_ID_MP3}
pattern AVCodecIdAac = AVCodecID #{const AV_CODEC_ID_AAC}
pattern AVCodecIdAc3 = AVCodecID #{const AV_CODEC_ID_AC3}
pattern AVCodecIdDts = AVCodecID #{const AV_CODEC_ID_DTS}
pattern AVCodecIdVorbis = AVCodecID #{const AV_CODEC_ID_VORBIS}
pattern AVCodecIdDvaudio = AVCodecID #{const AV_CODEC_ID_DVAUDIO}
pattern AVCodecIdWmav1 = AVCodecID #{const AV_CODEC_ID_WMAV1}
pattern AVCodecIdWmav2 = AVCodecID #{const AV_CODEC_ID_WMAV2}
pattern AVCodecIdMace3 = AVCodecID #{const AV_CODEC_ID_MACE3}
pattern AVCodecIdMace6 = AVCodecID #{const AV_CODEC_ID_MACE6}
pattern AVCodecIdVmdaudio = AVCodecID #{const AV_CODEC_ID_VMDAUDIO}
pattern AVCodecIdFlac = AVCodecID #{const AV_CODEC_ID_FLAC}
pattern AVCodecIdMp3adu = AVCodecID #{const AV_CODEC_ID_MP3ADU}
pattern AVCodecIdMp3on4 = AVCodecID #{const AV_CODEC_ID_MP3ON4}
pattern AVCodecIdShorten = AVCodecID #{const AV_CODEC_ID_SHORTEN}
pattern AVCodecIdAlac = AVCodecID #{const AV_CODEC_ID_ALAC}
pattern AVCodecIdWestwoodSnd1 = AVCodecID #{const AV_CODEC_ID_WESTWOOD_SND1}
pattern AVCodecIdGsm = AVCodecID #{const AV_CODEC_ID_GSM}
pattern AVCodecIdQdm2 = AVCodecID #{const AV_CODEC_ID_QDM2}
pattern AVCodecIdCook = AVCodecID #{const AV_CODEC_ID_COOK}
pattern AVCodecIdTruespeech = AVCodecID #{const AV_CODEC_ID_TRUESPEECH}
pattern AVCodecIdTta = AVCodecID #{const AV_CODEC_ID_TTA}
pattern AVCodecIdSmackaudio = AVCodecID #{const AV_CODEC_ID_SMACKAUDIO}
pattern AVCodecIdQcelp = AVCodecID #{const AV_CODEC_ID_QCELP}
pattern AVCodecIdWavpack = AVCodecID #{const AV_CODEC_ID_WAVPACK}
pattern AVCodecIdDsicinaudio = AVCodecID #{const AV_CODEC_ID_DSICINAUDIO}
pattern AVCodecIdImc = AVCodecID #{const AV_CODEC_ID_IMC}
pattern AVCodecIdMusepack7 = AVCodecID #{const AV_CODEC_ID_MUSEPACK7}
pattern AVCodecIdMlp = AVCodecID #{const AV_CODEC_ID_MLP}
pattern AVCodecIdGsmMs = AVCodecID #{const AV_CODEC_ID_GSM_MS}
pattern AVCodecIdAtrac3 = AVCodecID #{const AV_CODEC_ID_ATRAC3}
pattern AVCodecIdApe = AVCodecID #{const AV_CODEC_ID_APE}
pattern AVCodecIdNellymoser = AVCodecID #{const AV_CODEC_ID_NELLYMOSER}
pattern AVCodecIdMusepack8 = AVCodecID #{const AV_CODEC_ID_MUSEPACK8}
pattern AVCodecIdSpeex = AVCodecID #{const AV_CODEC_ID_SPEEX}
pattern AVCodecIdWmavoice = AVCodecID #{const AV_CODEC_ID_WMAVOICE}
pattern AVCodecIdWmapro = AVCodecID #{const AV_CODEC_ID_WMAPRO}
pattern AVCodecIdWmalossless = AVCodecID #{const AV_CODEC_ID_WMALOSSLESS}
pattern AVCodecIdAtrac3p = AVCodecID #{const AV_CODEC_ID_ATRAC3P}
pattern AVCodecIdEac3 = AVCodecID #{const AV_CODEC_ID_EAC3}
pattern AVCodecIdSipr = AVCodecID #{const AV_CODEC_ID_SIPR}
pattern AVCodecIdMp1 = AVCodecID #{const AV_CODEC_ID_MP1}
pattern AVCodecIdTwinvq = AVCodecID #{const AV_CODEC_ID_TWINVQ}
pattern AVCodecIdTruehd = AVCodecID #{const AV_CODEC_ID_TRUEHD}
pattern AVCodecIdMp4als = AVCodecID #{const AV_CODEC_ID_MP4ALS}
pattern AVCodecIdAtrac1 = AVCodecID #{const AV_CODEC_ID_ATRAC1}
pattern AVCodecIdBinkaudioRdft = AVCodecID #{const AV_CODEC_ID_BINKAUDIO_RDFT}
pattern AVCodecIdBinkaudioDct = AVCodecID #{const AV_CODEC_ID_BINKAUDIO_DCT}
pattern AVCodecIdAacLatm = AVCodecID #{const AV_CODEC_ID_AAC_LATM}
pattern AVCodecIdQdmc = AVCodecID #{const AV_CODEC_ID_QDMC}
pattern AVCodecIdCelt = AVCodecID #{const AV_CODEC_ID_CELT}
pattern AVCodecIdG723_1 = AVCodecID #{const AV_CODEC_ID_G723_1}
pattern AVCodecIdG729 = AVCodecID #{const AV_CODEC_ID_G729}
pattern AVCodecId8svxExp = AVCodecID #{const AV_CODEC_ID_8SVX_EXP}
pattern AVCodecId8svxFib = AVCodecID #{const AV_CODEC_ID_8SVX_FIB}
pattern AVCodecIdBmvAudio = AVCodecID #{const AV_CODEC_ID_BMV_AUDIO}
pattern AVCodecIdRalf = AVCodecID #{const AV_CODEC_ID_RALF}
pattern AVCodecIdIac = AVCodecID #{const AV_CODEC_ID_IAC}
pattern AVCodecIdIlbc = AVCodecID #{const AV_CODEC_ID_ILBC}
pattern AVCodecIdOpusDeprecated = AVCodecID #{const AV_CODEC_ID_OPUS_DEPRECATED}
pattern AVCodecIdComfortNoise = AVCodecID #{const AV_CODEC_ID_COMFORT_NOISE}
pattern AVCodecIdTakDeprecated = AVCodecID #{const AV_CODEC_ID_TAK_DEPRECATED}
pattern AVCodecIdMetasound = AVCodecID #{const AV_CODEC_ID_METASOUND}
pattern AVCodecIdPafAudioDeprecated = AVCodecID #{const AV_CODEC_ID_PAF_AUDIO_DEPRECATED}
pattern AVCodecIdOn2avc = AVCodecID #{const AV_CODEC_ID_ON2AVC}
pattern AVCodecIdFfwavesynth = AVCodecID #{const AV_CODEC_ID_FFWAVESYNTH}
pattern AVCodecIdSonic = AVCodecID #{const AV_CODEC_ID_SONIC}
pattern AVCodecIdSonicLs = AVCodecID #{const AV_CODEC_ID_SONIC_LS}
pattern AVCodecIdPafAudio = AVCodecID #{const AV_CODEC_ID_PAF_AUDIO}
pattern AVCodecIdOpus = AVCodecID #{const AV_CODEC_ID_OPUS}
pattern AVCodecIdTak = AVCodecID #{const AV_CODEC_ID_TAK}
pattern AVCodecIdEvrc = AVCodecID #{const AV_CODEC_ID_EVRC}
pattern AVCodecIdSmv = AVCodecID #{const AV_CODEC_ID_SMV}
pattern AVCodecIdDsdLsbf = AVCodecID #{const AV_CODEC_ID_DSD_LSBF}
pattern AVCodecIdDsdMsbf = AVCodecID #{const AV_CODEC_ID_DSD_MSBF}
pattern AVCodecIdDsdLsbfPlanar = AVCodecID #{const AV_CODEC_ID_DSD_LSBF_PLANAR}
pattern AVCodecIdDsdMsbfPlanar = AVCodecID #{const AV_CODEC_ID_DSD_MSBF_PLANAR}
pattern AVCodecIdFirstSubtitle = AVCodecID #{const AV_CODEC_ID_FIRST_SUBTITLE}
pattern AVCodecIdDvdSubtitle = AVCodecID #{const AV_CODEC_ID_DVD_SUBTITLE}
pattern AVCodecIdDvbSubtitle = AVCodecID #{const AV_CODEC_ID_DVB_SUBTITLE}
pattern AVCodecIdText = AVCodecID #{const AV_CODEC_ID_TEXT}
pattern AVCodecIdXsub = AVCodecID #{const AV_CODEC_ID_XSUB}
pattern AVCodecIdSsa = AVCodecID #{const AV_CODEC_ID_SSA}
pattern AVCodecIdMovText = AVCodecID #{const AV_CODEC_ID_MOV_TEXT}
pattern AVCodecIdHdmvPgsSubtitle = AVCodecID #{const AV_CODEC_ID_HDMV_PGS_SUBTITLE}
pattern AVCodecIdDvbTeletext = AVCodecID #{const AV_CODEC_ID_DVB_TELETEXT}
pattern AVCodecIdSrt = AVCodecID #{const AV_CODEC_ID_SRT}
pattern AVCodecIdMicrodvd = AVCodecID #{const AV_CODEC_ID_MICRODVD}
pattern AVCodecIdEia608 = AVCodecID #{const AV_CODEC_ID_EIA_608}
pattern AVCodecIdJacosub = AVCodecID #{const AV_CODEC_ID_JACOSUB}
pattern AVCodecIdSami = AVCodecID #{const AV_CODEC_ID_SAMI}
pattern AVCodecIdRealtext = AVCodecID #{const AV_CODEC_ID_REALTEXT}
pattern AVCodecIdStl = AVCodecID #{const AV_CODEC_ID_STL}
pattern AVCodecIdSubviewer1 = AVCodecID #{const AV_CODEC_ID_SUBVIEWER1}
pattern AVCodecIdSubviewer = AVCodecID #{const AV_CODEC_ID_SUBVIEWER}
pattern AVCodecIdSubrip = AVCodecID #{const AV_CODEC_ID_SUBRIP}
pattern AVCodecIdWebvtt = AVCodecID #{const AV_CODEC_ID_WEBVTT}
pattern AVCodecIdMpl2 = AVCodecID #{const AV_CODEC_ID_MPL2}
pattern AVCodecIdVplayer = AVCodecID #{const AV_CODEC_ID_VPLAYER}
pattern AVCodecIdPjs = AVCodecID #{const AV_CODEC_ID_PJS}
pattern AVCodecIdAss = AVCodecID #{const AV_CODEC_ID_ASS}
pattern AVCodecIdFirstUnknown = AVCodecID #{const AV_CODEC_ID_FIRST_UNKNOWN}
pattern AVCodecIdTtf = AVCodecID #{const AV_CODEC_ID_TTF}
pattern AVCodecIdBintext = AVCodecID #{const AV_CODEC_ID_BINTEXT}
pattern AVCodecIdXbin = AVCodecID #{const AV_CODEC_ID_XBIN}
pattern AVCodecIdIdf = AVCodecID #{const AV_CODEC_ID_IDF}
pattern AVCodecIdOtf = AVCodecID #{const AV_CODEC_ID_OTF}
pattern AVCodecIdSmpteKlv = AVCodecID #{const AV_CODEC_ID_SMPTE_KLV}
pattern AVCodecIdDvdNav = AVCodecID #{const AV_CODEC_ID_DVD_NAV}
pattern AVCodecIdTimedId3 = AVCodecID #{const AV_CODEC_ID_TIMED_ID3}
pattern AVCodecIdBinData = AVCodecID #{const AV_CODEC_ID_BIN_DATA}
pattern AVCodecIdProbe = AVCodecID #{const AV_CODEC_ID_PROBE}
pattern AVCodecIdMpeg2ts = AVCodecID #{const AV_CODEC_ID_MPEG2TS}
pattern AVCodecIdMpeg4systems = AVCodecID #{const AV_CODEC_ID_MPEG4SYSTEMS}
pattern AVCodecIdFfmetadata = AVCodecID #{const AV_CODEC_ID_FFMETADATA}

-- AV_CODEC_ID_HQX
-- AV_CODEC_ID_TDSC
-- AV_CODEC_ID_HQ_HQA
-- AV_CODEC_ID_HAP
-- AV_CODEC_ID_DDS
-- AV_CODEC_ID_PCM_S16BE_PLANAR_DEPRECATED
-- AV_CODEC_ID_ADPCM_THP_LE
-- AV_CODEC_ID_DSS_SP
-- AV_CODEC_ID_4GV

