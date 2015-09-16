{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Core (
	AVFormatContext,
	addAVFormatContextFinalizer,
	AVInputFormat(..),
	AVOutputFormat(..),
	AVStream,
	AVProgram,

	getStreams,
	withStream,
	withStreamCodecContext,
	getPrograms,

	avformatConfiguration,
	avformatLicense,
	registerAll,
	registerInputFormat,
	registerOutputFormat,
	networkInit,
	networkDeInit,
	inputFormats,
	outputFormats,
	mkAVFormatContext,
	mkAVFormatOutputContext,
	mkAVFormatContextFromPtr,
	newStream,
	AVPacketSideDataPayload,
	streamGetSideData,
	newProgram,

	TagTable(..),
	codecGetID,
	codecGetTag,
	findDefaultStreamIndex,
	dumpInputFormat,
	dumpOutputFormat,
	dumpFormat,
	queryCodec,
	guessSampleAspectRatio,
	guessFrameRate,
	getStreamRFrameRate,
	setStreamRFrameRate,
	getStreamRecommendedEncoderConfiguration,
	setStreamRecommendedEncoderConfiguration,
	getStreamEndPTS,

	formatGetProbeScore,
	formatGetVideoCodec,
	formatSetVideoCodec,
	formatGetAudioCodec,
	formatSetAudioCodec,
	formatGetSubtitleCodec,
	formatSetSubtitleCodec,
	--formatGetDataCodec,
	--formatSetDataCodec,
	formatGetMetadataHeaderPadding,
	formatSetMetadataHeaderPadding,
	formatInjectGlobalSideData,
	formatGetDurationEstimationMethod
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.Ratio
import Data.Traversable (traverse)
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec
import Media.FFMpeg.Codec.Core
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- | AVFormatContext struct
newtype AVFormatContext = AVFormatContext (ForeignPtr AVFormatContext)
instance ExternalPointer AVFormatContext where
	type UnderlyingType AVFormatContext = AVFormatContext
	withThis (AVFormatContext fp) = withThis fp
instance ReflectClass AVFormatContext where
	withClass ctx action = withThis ctx$ \pctx ->
		action =<< (liftIO$ avClassFromPtr <$> #{peek AVFormatContext, av_class} pctx)
instance HasClass AVFormatContext where
	getClass = avClassFromPtr avformat_get_class

addAVFormatContextFinalizer (AVFormatContext fp) f = liftIO$ addForeignPtrFinalizer f fp

-- | AVInputFormat struct
newtype AVInputFormat = AVInputFormat (Ptr AVInputFormat) -- Ptr is OK because these are never freed
instance ExternalPointer AVInputFormat where
	type UnderlyingType AVInputFormat = AVInputFormat
	withThis (AVInputFormat ptr) = withThis ptr

-- | AVOutputFormat struct
newtype AVOutputFormat = AVOutputFormat (Ptr AVOutputFormat) -- Ptr is OK because these are never freed
instance ExternalPointer AVOutputFormat where
	type UnderlyingType AVOutputFormat = AVOutputFormat
	withThis (AVOutputFormat ptr) = withThis ptr

-- | AVStream struct
newtype AVStream = AVStream (Ptr AVStream) -- Not safe.  All access to AVStream needs to be bracketed
instance ExternalPointer AVStream where
	type UnderlyingType AVStream = AVStream
	withThis (AVStream ptr) = withThis ptr

-- | AVProgram struct
newtype AVProgram = AVProgram (Ptr AVProgram) -- Not safe.  All access to AVProgram needs to be bracketed
instance ExternalPointer AVProgram where
	type UnderlyingType AVProgram = AVProgram
	withThis (AVProgram ptr) = withThis ptr

foreign import ccall "avformat_version" avformat_version :: IO CUInt
foreign import ccall "avformat_configuration" avformat_configuration :: IO CString
foreign import ccall "avformat_license" avformat_license :: IO CString
foreign import ccall "av_register_all" av_register_all :: IO ()
foreign import ccall "av_register_input_format" av_register_input_format :: Ptr AVInputFormat -> IO ()
foreign import ccall "av_register_output_format" av_register_output_format :: Ptr AVOutputFormat -> IO ()
foreign import ccall "avformat_network_init" avformat_network_init :: IO CInt
foreign import ccall "avformat_network_deinit" avformat_network_deinit :: IO CInt
foreign import ccall "av_iformat_next" av_iformat_next :: Ptr AVInputFormat -> IO (Ptr AVInputFormat)
foreign import ccall "av_oformat_next" av_oformat_next :: Ptr AVOutputFormat -> IO (Ptr AVOutputFormat)
foreign import ccall "avformat_alloc_context" avformat_alloc_context :: IO (Ptr AVFormatContext)
foreign import ccall "avformat_free_context" avformat_free_context :: Ptr AVFormatContext -> IO ()
foreign import ccall "&avformat_free_context" pavformat_free_context :: FunPtr (Ptr AVFormatContext -> IO ())
foreign import ccall "avformat_get_class" avformat_get_class :: Ptr (AVClass AVFormatContext)
foreign import ccall "avformat_new_stream" avformat_new_stream :: Ptr AVFormatContext -> Ptr AVCodec -> IO (Ptr AVStream)
foreign import ccall "av_stream_get_side_data" av_stream_get_side_data :: Ptr AVStream -> CInt -> Ptr CInt -> IO (Ptr Word8)
foreign import ccall "av_new_program" av_new_program :: Ptr AVFormatContext -> CInt -> IO (Ptr AVProgram)

foreign import ccall "av_hex_dump" av_hex_dump :: Ptr CFile -> Ptr Word8 -> CInt -> IO ()
foreign import ccall "av_hex_dump_log" av_hex_dump_log :: Ptr () -> CInt -> Ptr Word8 -> CInt -> IO ()
foreign import ccall "av_pkt_dump2" av_pkt_dump2 :: Ptr CFile -> Ptr AVPacket -> CInt -> Ptr AVStream -> IO ()
foreign import ccall "av_pkt_dump_log2" av_pkt_dump_log2 :: Ptr () -> CInt -> Ptr AVPacket -> CInt -> Ptr AVStream -> IO ()
foreign import ccall "av_codec_get_id" av_codec_get_id :: Ptr (Ptr ()) -> CUInt -> IO CInt
foreign import ccall "av_codec_get_tag" av_codec_get_tag :: Ptr (Ptr ()) -> CInt -> IO CUInt
foreign import ccall "av_codec_get_tag2" av_codec_get_tag2 :: Ptr (Ptr ()) -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "av_find_default_stream_index" av_find_default_stream_index :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_index_search_timestamp" av_index_search_timestamp :: Ptr AVStream -> Int64 -> CInt -> IO CInt
foreign import ccall "av_add_index_entry" av_add_index_entry :: Ptr AVStream -> Int64 -> Int64 -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "av_url_split" av_url_split :: CString -> CInt -> CString -> CInt -> CString ->
	CInt -> Ptr CInt -> CString -> CInt -> CString -> IO ()
foreign import ccall "av_dump_format" av_dump_format :: Ptr AVFormatContext -> CInt -> CString -> CInt -> IO ()
foreign import ccall "av_get_frame_filename" av_get_frame_filename :: CString -> CInt -> CString -> CInt -> IO CInt
foreign import ccall "av_filename_number_test" av_filename_number_test :: CString -> IO CInt
foreign import ccall "av_sdp_create" av_sdp_create :: Ptr AVFormatContext -> CInt -> CString -> CInt -> IO CInt
foreign import ccall "av_match_ext" av_match_ext :: CString -> CString -> IO CInt
foreign import ccall "avformat_query_codec" avformat_query_codec :: Ptr AVOutputFormat -> CInt -> CInt -> IO CInt
foreign import ccall "b_av_guess_sample_aspect_ratio" av_guess_sample_aspect_ratio ::
	Ptr AVFormatContext -> Ptr AVStream -> Ptr AVFrame -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "av_guess_frame_rate" av_guess_frame_rate :: 
	Ptr AVFormatContext -> Ptr AVStream -> Ptr AVFrame -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "avformat_match_stream_specifier" avformat_match_stream_specifier :: Ptr AVFormatContext -> Ptr AVStream -> CString -> IO CInt
foreign import ccall "avformat_queue_attached_pictures" avformat_queue_attached_pictures :: Ptr AVFormatContext -> IO CInt

foreign import ccall "b_av_stream_get_r_frame_rate" av_stream_get_r_frame_rate :: Ptr AVStream -> Ptr (Maybe AVRational) -> IO ()
foreign import ccall "b_av_stream_set_r_frame_rate" av_stream_set_r_frame_rate :: Ptr AVStream -> Ptr (Maybe AVRational) -> IO ()
foreign import ccall "av_stream_get_parser" av_stream_get_parser :: AVStream -> IO (Ptr ())
foreign import ccall "av_stream_get_recommended_encoder_configuration" av_stream_get_recommended_encoder_configuration :: Ptr AVStream -> IO CString
foreign import ccall "av_stream_set_recommended_encoder_configuration" av_stream_set_recommended_encoder_configuration :: Ptr AVStream -> CString -> IO ()
foreign import ccall "av_stream_get_end_pts" av_stream_get_end_pts :: Ptr AVStream -> IO Int64

foreign import ccall "av_format_get_probe_score" av_format_get_probe_score :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_format_get_video_codec" av_format_get_video_codec :: Ptr AVFormatContext -> IO (Ptr AVCodec)
foreign import ccall "av_format_set_video_codec" av_format_set_video_codec :: Ptr AVFormatContext -> Ptr AVCodec -> IO ()
foreign import ccall "av_format_get_audio_codec" av_format_get_audio_codec :: Ptr AVFormatContext -> IO (Ptr AVCodec)
foreign import ccall "av_format_set_audio_codec" av_format_set_audio_codec :: Ptr AVFormatContext -> Ptr AVCodec -> IO ()
foreign import ccall "av_format_get_subtitle_codec" av_format_get_subtitle_codec :: Ptr AVFormatContext -> IO (Ptr AVCodec)
foreign import ccall "av_format_set_subtitle_codec" av_format_set_subtitle_codec :: Ptr AVFormatContext -> Ptr AVCodec -> IO ()
foreign import ccall "av_format_get_data_codec" av_format_get_data_codec :: Ptr AVFormatContext -> IO (Ptr AVCodec)
foreign import ccall "av_format_set_data_codec" av_format_set_data_codec :: Ptr AVFormatContext -> Ptr AVCodec -> IO ()
foreign import ccall "av_format_get_metadata_header_padding" av_format_get_metadata_header_padding :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_format_set_metadata_header_padding" av_format_set_metadata_header_padding :: Ptr AVFormatContext -> CInt -> IO ()
foreign import ccall "av_format_inject_global_side_data" av_format_inject_global_side_data :: Ptr AVFormatContext -> IO ()
foreign import ccall "av_fmt_ctx_get_duration_estimation_method" av_fmt_ctx_get_duration_estimation_method :: Ptr AVFormatContext -> IO CInt

foreign import ccall "avformat_alloc_output_context2" avformat_alloc_output_context2 ::
	Ptr (Ptr AVFormatContext) -> Ptr AVOutputFormat -> CString -> CString -> IO CInt

-- | Get the stream indexes from a format context
getStreams :: MonadIO m => AVFormatContext -> m [StreamIndex]
getStreams ctx = do
	nbStreams <- liftIO.withThis ctx$ \pctx ->
		(#{peek AVFormatContext, nb_streams} pctx :: IO CUInt)
	return$ (StreamIndex).fromIntegral <$>
		if nbStreams == 0 then [] else [0..(nbStreams - 1)]

-- | Perform an action with an AVStream
withStream :: (MonadIO m, MonadThrow m) =>
	AVFormatContext -> StreamIndex -> (AVStream -> m b) -> m b
withStream ctx (StreamIndex idx) action = withThis ctx$ \pctx -> do
	ns <- liftIO$ #{peek AVFormatContext, nb_streams} pctx
	when (idx < 0 || idx >= ns)$ throwM$
		HSFFError HSFFErrorInvalidStreamIndex "withStream" (show idx)
	pstreams <- liftIO$ #{peek AVFormatContext, streams} pctx
	action =<< (liftIO$ AVStream <$> peekElemOff pstreams (fromIntegral idx))

-- | Perform an action using the AVCodecContext associated with an AVStream
withStreamCodecContext :: MonadIO m => AVStream -> (AVCodecContext -> m b) -> m b
withStreamCodecContext s action = withThis s$ \ps -> do
	fp <- liftIO$ mallocForeignPtr
	liftIO.withForeignPtr fp$ \ppctx -> poke ppctx =<< (#{peek AVStream, codec} ps)
	action$ AVCodecContext fp

-- | Get all the programs from a format context
getPrograms :: MonadIO m => AVFormatContext -> m [ProgramID]
getPrograms ctx = withThis ctx$ \pctx -> liftIO$ do
	nb_programs <- #{peek AVFormatContext, nb_programs} pctx :: IO CUInt
	ppro <- liftIO$ #{peek AVFormatContext, programs} pctx
	forM (if nb_programs == 0 then [] else [0..(nb_programs - 1)])$ \i ->
		ProgramID <$> (#{peek AVProgram, id} =<< peekElemOff ppro (fromIntegral i))

-- | The avformat configuration string
avformatConfiguration :: String
avformatConfiguration =
	-- safe because avformat_configuration returns a const string
	unsafePerformIO$ peekCString =<< avformat_configuration

-- | The avformat license string
avformatLicense :: String
avformatLicense =
	-- safe because avformat_license returns a const string
	unsafePerformIO$ peekCString =<< avformat_license

-- | Register all input and output formats
registerAll :: MonadIO m => m ()
registerAll = liftIO av_register_all

-- | Register an input format
registerInputFormat :: MonadIO m => AVInputFormat -> m ()
registerInputFormat inf = withThis inf$ liftIO.av_register_input_format

-- | Register an output format
registerOutputFormat :: MonadIO m => AVOutputFormat -> m ()
registerOutputFormat ouf = withThis ouf$ liftIO.av_register_output_format

-- | Init networking
networkInit :: MonadIO m => m ()
networkInit = liftIO$ avformat_network_init >> return ()

-- | Deinit networking
networkDeInit :: MonadIO m => m ()
networkDeInit = liftIO$ avformat_network_deinit >> return ()

-- | Get all the registered input formats
inputFormats :: MonadIO m => m [AVInputFormat]
inputFormats = liftIO$ (fmap AVInputFormat) <$> allInputFormats nullPtr
	where
		allInputFormats prev = do
			next <- av_iformat_next prev
			if next == nullPtr then return [] else (next :) <$> allInputFormats next

-- | Get all the registered output formats
outputFormats :: MonadIO m => m [AVOutputFormat]
outputFormats = liftIO$ (fmap AVOutputFormat) <$> allOutputFormats nullPtr
	where
		allOutputFormats prev = do
			next <- av_oformat_next prev
			if next == nullPtr then return [] else (next :) <$> allOutputFormats next

-- | Allocate a new AVFormatContext with finalization
mkAVFormatContext :: (MonadIO m, MonadThrow m) => m AVFormatContext
mkAVFormatContext = do
	ptr <- liftIO$ avformat_alloc_context
	if ptr == nullPtr
		then throwM$
			mkNullPointerError "mkAVFormatContext" "avformat_alloc_context"
		else mkAVFormatContextFromPtr ptr Nothing

-- | Allocate a new AVFormatContext for output.  At least one of the three
-- parameters must be Just.
mkAVFormatOutputContext :: (MonadIO m, MonadThrow m) =>
	Maybe AVOutputFormat   -- Output format to allocate the context with
	-> Maybe String        -- Format name
	-> Maybe String        -- File name
	-> m AVFormatContext
mkAVFormatOutputContext oformat formatName filename =
	withOrNull oformat$ \poformat ->
	withOrNull formatName$ \pformatName ->
	withOrNull filename$ \pfilename -> do
		(pctx, r) <- liftIO.alloca$ \ppctx -> do
			r <- avformat_alloc_output_context2 ppctx poformat pformatName pfilename
			pctx <- peek ppctx
			return (pctx, r)
		when (r < 0)$ throwM$
			mkError r "mkAVFormatOutputContext" "avformat_alloc_output_context2"
		when (pctx == nullPtr)$ throwM$ mkNullPointerError
			"mkAVFormatOutputContext" "avformat_alloc_output_context2"
		mkAVFormatContextFromPtr pctx Nothing

-- | Make an AVFormatContext given a pointer
mkAVFormatContextFromPtr :: MonadIO m =>
	Ptr AVFormatContext -> Maybe (FunPtr (Ptr () -> IO ())) -> m AVFormatContext
mkAVFormatContextFromPtr ptr mf = liftIO$ do
	fp <- newForeignPtr pavformat_free_context ptr
	case mf of
		Just f -> addForeignPtrFinalizer f (castForeignPtr fp)
		Nothing -> return ()

	return$ AVFormatContext fp

-- | Create a new stream and add it to a format context
newStream :: (MonadIO m, MonadThrow m) =>
	AVFormatContext -> Maybe AVCodec -> m StreamIndex
newStream ctx mcd = do
	r <- liftIO.withThis ctx$ \pctx ->
		case mcd of
			Just cd -> withThis cd (avformat_new_stream pctx)
			Nothing -> avformat_new_stream pctx nullPtr

	if r == nullPtr then throwM$
		mkNullPointerError "newStream" "avformat_new_stream"
	else liftIO$ StreamIndex <$> #{peek AVStream, index} r

peekAVStreamSideDataPtrType :: AVPacketSideDataType -> Ptr AVStream -> IO (Ptr Word8, Int)
peekAVStreamSideDataPtrType sdType ptr =
	alloca $ \psize -> do
		pdata <- av_stream_get_side_data ptr (fromCEnum sdType) psize
		size <- fromIntegral <$> peek psize
		return (pdata, size)

class AVPacketSideDataPayload a => AVStreamSideDataPayload a where
	peekAVStreamSideDataPtr :: Ptr AVStream -> IO (Ptr a, Int)

instance AVStreamSideDataPayload AVPacketSideDataPalette where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataPalette
instance AVStreamSideDataPayload AVPacketSideDataNewExtradata where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataNewExtradata
instance AVStreamSideDataPayload AVPacketSideDataParamChange where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataParamChange
instance AVStreamSideDataPayload AVPacketSideDataH263MbInfo where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataH263MbInfo
instance AVStreamSideDataPayload AVPacketSideDataReplayGain where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataReplayGain
instance AVStreamSideDataPayload AVPacketSideDataDisplayMatrix where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataDisplayMatrix
instance AVStreamSideDataPayload AVPacketSideDataStereo3d where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataStereo3d
instance AVStreamSideDataPayload AVPacketSideDataSkipSamples where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataSkipSamples
instance AVStreamSideDataPayload AVPacketSideDataJpDualmono where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataJpDualmono
instance AVStreamSideDataPayload AVPacketSideDataStringsMetadata where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataStringsMetadata
instance AVStreamSideDataPayload AVPacketSideDataSubtitlePosition where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataSubtitlePosition
instance AVStreamSideDataPayload AVPacketSideDataMatroskaBlockadditional where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataMatroskaBlockadditional
instance AVStreamSideDataPayload AVPacketSideDataWebvttIdentifier where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataWebvttIdentifier
instance AVStreamSideDataPayload AVPacketSideDataWebvttSettings where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataWebvttSettings
instance AVStreamSideDataPayload AVPacketSideDataMetadataUpdate where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType AVPktDataMetadataUpdate

-- | Get side data associated with an AVStream
streamGetSideData :: forall a m. (MonadIO m, AVStreamSideDataPayload a) =>
	AVStream -> m (Maybe (AVPacketSideData a))
streamGetSideData pkt = liftIO.withThis pkt$ \ptr -> do
	(pdata, dsize) <- peekAVStreamSideDataPtr ptr :: IO (Ptr a, Int)
	if (pdata == nullPtr) then return Nothing else do
		v <- peekPayload pdata (fromIntegral dsize)
		return.Just$ AVPacketSideData v

-- | Create a new program with the given id in an AVFormatContext
newProgram :: (MonadIO m, MonadThrow m) => AVFormatContext -> Int -> m ()
newProgram ctx pid = do
	r <- liftIO.withThis ctx$ \pctx -> av_new_program pctx (fromIntegral pid)

	when (r == nullPtr)$ throwM$ mkNullPointerError "newProgram" "av_new_program"

foreign import ccall "avformat_get_riff_video_tags" avformat_get_riff_video_tags :: IO (Ptr ())
foreign import ccall "avformat_get_riff_audio_tags" avformat_get_riff_audio_tags :: IO (Ptr ())
foreign import ccall "avformat_get_mov_video_tags" avformat_get_mov_video_tags :: IO (Ptr ())
foreign import ccall "avformat_get_mov_audio_tags" avformat_get_mov_audio_tags :: IO (Ptr ())

-- | Enumeration of tag tables
data TagTable = RIFFVideo | RIFFAudio | MOVVideo | MOVAudio deriving (Eq, Show, Enum)

-- | Get a pointer to a tag table
getTable :: TagTable -> Ptr ()
getTable RIFFVideo = unsafePerformIO$ avformat_get_riff_video_tags
getTable RIFFAudio = unsafePerformIO$ avformat_get_riff_audio_tags
getTable MOVVideo = unsafePerformIO$ avformat_get_mov_video_tags
getTable MOVAudio = unsafePerformIO$ avformat_get_mov_audio_tags

-- | Get the codec ID associated with a tag
codecGetID :: TagTable -> Word -> Maybe AVCodecID
codecGetID table tag = unsafePerformIO$ do
	cid <- with (getTable table)$ \ptr ->
		toCEnum.fromIntegral <$> av_codec_get_id ptr (fromIntegral tag)
	if cid == AVCodecIdNone then return Nothing else return$ Just cid

-- | Get the tag associated with a codec
codecGetTag :: TagTable -> AVCodecID -> Maybe Word
codecGetTag table cid = unsafePerformIO$ do
	(r, tag) <-
		with (getTable table)$ \ptr ->
		alloca$ \ptag -> do
			r <- av_codec_get_tag2 ptr (fromCEnum cid) ptag
			tag <- peek ptag
			return (r, fromIntegral tag)
	
	if r == 0 then return Nothing else return$ Just tag

-- | Get the index of the main stream in a media file
findDefaultStreamIndex :: MonadIO m => AVFormatContext -> m StreamIndex
findDefaultStreamIndex ctx = liftIO.withThis ctx$
	(fmap StreamIndex).av_find_default_stream_index

-- | Dump format information for all input streams
dumpInputFormat :: MonadIO m => AVFormatContext -> FilePath -> m ()
dumpInputFormat = dumpFormat False

-- | Dump format information for all output streams
dumpOutputFormat :: MonadIO m => AVFormatContext -> FilePath -> m ()
dumpOutputFormat = dumpFormat True

dumpFormat :: MonadIO m => Bool -> AVFormatContext -> FilePath -> m ()
dumpFormat isOutput fmt s = liftIO$
	withThis fmt $ \fmt' ->
	withCString s $ \s' -> do
		count <- fromIntegral <$>
			(#{peek AVFormatContext, nb_streams} fmt' :: IO CUInt)
		forM_ [1..count] $ \i -> av_dump_format fmt' i s' io
	where io = if isOutput then 1 else 0

-- | Determine if a given output format can accommodate the given codec
queryCodec :: MonadIO m =>
	AVOutputFormat -> AVCodecID -> FFCompliance -> m (Maybe Bool)
queryCodec ouf cid compliance = do
	r <- liftIO.withThis ouf$ \po ->
		avformat_query_codec po (fromCEnum cid) (fromCEnum compliance)

	return$ case r of
		0 -> Just False
		1 -> Just True
		_ -> Nothing

-- | Guess the sample aspect ratio of a given frame within a given stream
-- within a given format context
guessSampleAspectRatio :: MonadIO m =>
	AVFormatContext -> AVStream -> AVFrame -> m (Maybe AVRational)
guessSampleAspectRatio ctx stream frame = liftIO$ do
	(num, den) <-
		withThis ctx$ \pctx ->
		withThis stream$ \pstream ->
		withThis frame$ \pframe ->
		alloca$ \pnum ->
		alloca$ \pden -> do
			av_guess_sample_aspect_ratio pctx pstream pframe pnum pden
			num <- peek pnum
			den <- peek pden
			return (fromIntegral num, fromIntegral den)
	
	if num == 0 && den == 1 then return Nothing
		else return . Just . AVRational$ num % den

-- | Guess the frame rate at a given frame within a given stream within a given
-- format context
guessFrameRate :: MonadIO m =>
	AVFormatContext -> AVStream -> AVFrame -> m (Maybe AVRational)
guessFrameRate ctx stream frame = liftIO$ do
	(num, den) <-
		withThis ctx$ \pctx ->
		withThis stream$ \pstream ->
		withThis frame$ \pframe ->
		alloca$ \pnum ->
		alloca$ \pden -> do
			av_guess_frame_rate pctx pstream pframe pnum pden
			num <- peek pnum
			den <- peek pden
			return (fromIntegral num, fromIntegral den)

	if num == 0 && den == 1 then return Nothing
		else return . Just . AVRational$ num % den

-- | Get the r_frame_rate field from an AVStream
getStreamRFrameRate :: MonadIO m => AVStream -> m (Maybe AVRational)
getStreamRFrameRate s =
	withThis s$ \ps ->
		liftIO.alloca$ \pr -> do
			av_stream_get_r_frame_rate ps pr
			peek pr

-- | Set the r_frame_rate field for an AVStream
setStreamRFrameRate :: MonadIO m => AVStream -> AVRational -> m ()
setStreamRFrameRate s v = withThis s$ \ps ->
	liftIO.with (Just v)$ \pv -> av_stream_set_r_frame_rate ps pv

-- | Get the recommended encoder configuration string
getStreamRecommendedEncoderConfiguration :: MonadIO m =>
	AVStream -> m (Maybe String)
getStreamRecommendedEncoderConfiguration s = liftIO.withThis s$ \ps ->
	((traverse peekCString).justPtr) =<<
		av_stream_get_recommended_encoder_configuration ps

-- | Set the recommended encoder configuration string for an AVStream
setStreamRecommendedEncoderConfiguration :: MonadIO m =>
	AVStream -> String -> m ()
setStreamRecommendedEncoderConfiguration s v =
	withThis s$ \ps ->
	withThis v$ \pv ->
		liftIO$ av_stream_set_recommended_encoder_configuration ps pv

-- | Get the end_pts field from an AVStream
getStreamEndPTS :: MonadIO m => AVStream -> m AVTimestamp
getStreamEndPTS s = withThis s$ \ps ->
	liftIO$ AVTimestamp <$> av_stream_get_end_pts ps

-- | Get the probe_score
formatGetProbeScore :: MonadIO m => AVFormatContext -> m Int
formatGetProbeScore ctx = liftIO$
	fromIntegral <$> withThis ctx av_format_get_probe_score

-- | Get the video codec
formatGetVideoCodec :: MonadIO m => AVFormatContext -> m (Maybe AVCodec)
formatGetVideoCodec ctx = liftIO$
	((fmap AVCodec).justPtr) <$> withThis ctx av_format_get_video_codec

-- | Set the video codec
formatSetVideoCodec :: MonadIO m => AVFormatContext -> AVCodec -> m ()
formatSetVideoCodec ctx cd =
	withThis ctx$ \pctx ->
	withThis cd$ \pcd -> liftIO$ av_format_set_video_codec pctx pcd

-- | Get the audio codec
formatGetAudioCodec :: MonadIO m => AVFormatContext -> m (Maybe AVCodec)
formatGetAudioCodec ctx = liftIO$
	((fmap AVCodec).justPtr) <$> withThis ctx av_format_get_audio_codec

-- | Set the audio codec
formatSetAudioCodec :: MonadIO m => AVFormatContext -> AVCodec -> m ()
formatSetAudioCodec ctx cd =
	withThis ctx$ \pctx ->
	withThis cd$ \pcd -> liftIO$ av_format_set_audio_codec pctx pcd

-- | Get the subtitle codec
formatGetSubtitleCodec :: MonadIO m => AVFormatContext -> m (Maybe AVCodec)
formatGetSubtitleCodec ctx = liftIO$
	((fmap AVCodec).justPtr) <$> withThis ctx av_format_get_subtitle_codec

-- | Set the subtitle codec
formatSetSubtitleCodec :: MonadIO m => AVFormatContext -> AVCodec -> m ()
formatSetSubtitleCodec ctx cd =
	withThis ctx$ \pctx ->
	withThis cd$ \pcd -> liftIO$ av_format_set_subtitle_codec pctx pcd

-- Get the data codec
-- formatGetDataCodec :: MonadIO m => AVFormatContext -> m (Maybe AVCodec)
-- formatGetDataCodec ctx = liftIO$ AVCodec <$> withThis ctx av_format_get_data_codec

-- Set the data codec
--formatSetDataCodec :: MonadIO m => AVFormatContext -> AVCodec -> m ()
--formatSetDataCodec ctx cd =
--	withThis ctx$ \pctx ->
--	withThis cd$ \pcd -> liftIO$ av_format_set_data_codec pctx pcd

-- | Get the metadata_header_padding field
formatGetMetadataHeaderPadding :: MonadIO m => AVFormatContext -> m Int
formatGetMetadataHeaderPadding ctx = liftIO$ fromIntegral <$>
	withThis ctx av_format_get_metadata_header_padding

-- | Set the metadata_header_padding field
formatSetMetadataHeaderPadding :: MonadIO m => AVFormatContext -> Int -> m ()
formatSetMetadataHeaderPadding ctx i = liftIO.withThis ctx$ \pctx ->
	av_format_set_metadata_header_padding pctx (fromIntegral i)

-- | Inject global side data into all subsequent packets
formatInjectGlobalSideData :: MonadIO m => AVFormatContext -> m ()
formatInjectGlobalSideData ctx = liftIO$
	withThis ctx av_format_inject_global_side_data

-- | Get the AVDurationEstimationMethod
formatGetDurationEstimationMethod :: MonadIO m =>
	AVFormatContext -> m AVDurationEstimationMethod
formatGetDurationEstimationMethod ctx = liftIO$ toCEnum <$>
	withThis ctx av_fmt_ctx_get_duration_estimation_method

