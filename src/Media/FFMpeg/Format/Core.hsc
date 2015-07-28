{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Core (
	AVFormatContext,
	AVInputFormat,
	AVOutputFormat,
	AVStream,
	AVProgram,

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
	guessFrameRate
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Except
import Data.Int
import Data.Ratio
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

-- | AVFormatContext struct
newtype AVFormatContext = AVFormatContext (ForeignPtr AVFormatContext)
instance ExternalPointer AVFormatContext where
	withThis (AVFormatContext fmt) io = withForeignPtr fmt (io . castPtr)

-- | AVInputFormat struct
newtype AVInputFormat = AVInputFormat (Ptr AVInputFormat) -- Ptr is OK because these are never freed
instance ExternalPointer AVInputFormat where
	withThis (AVInputFormat ptr) io = io.castPtr$ ptr

-- | AVOutputFormat struct
newtype AVOutputFormat = AVOutputFormat (Ptr AVOutputFormat) -- Ptr is OK because these are never freed
instance ExternalPointer AVOutputFormat where
	withThis (AVOutputFormat ptr) io = io.castPtr$ ptr

-- | AVStream struct
newtype AVStream = AVStream (Ptr AVStream) -- Not safe.  All access to AVStream needs to be bracketed
instance ExternalPointer AVStream where
	withThis (AVStream ptr) io = io.castPtr$ ptr

-- | AVProgram struct
newtype AVProgram = AVProgram (Ptr AVProgram) -- Not safe.  All access to AVProgram needs to be bracketed
instance ExternalPointer AVProgram where
	withThis (AVProgram ptr) io = io.castPtr$ ptr

-- | Type for stream indexes
newtype StreamIndex = StreamIndex CInt

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
--foreign import ccall "avformat_get_class" avformat_get_class :: IO (Ptr AVClass)
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
registerInputFormat inf = liftIO.withThis inf$ av_register_input_format

-- | Register an output format
registerOutputFormat :: MonadIO m => AVOutputFormat -> m ()
registerOutputFormat ouf = liftIO.withThis ouf$ av_register_output_format

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
mkAVFormatContext :: (MonadIO m, MonadError String m) => m AVFormatContext
mkAVFormatContext = do
	ptr <- liftIO$ avformat_alloc_context
	if ptr == nullPtr
		then throwError$ "mkAVFormatContext: avformat_alloc_context returned a null pointer"
		else liftIO$ AVFormatContext <$> newForeignPtr pavformat_free_context ptr

-- | Create a new stream and add it to a format context
newStream :: (MonadIO m, MonadError String m) => AVFormatContext -> Maybe AVCodec -> m ()
newStream ctx mcd = do
	r <- liftIO.withThis ctx$ \pctx ->
		case mcd of
			Just cd -> withThis cd (avformat_new_stream pctx)
			Nothing -> avformat_new_stream pctx nullPtr

	when (r == nullPtr)$
		throwError "newStream: avformat_new_stream returned a null pointer"

peekAVStreamSideDataPtrType :: AVPacketSideDataType -> Ptr AVStream -> IO (Ptr Word8, Int)
peekAVStreamSideDataPtrType sdType ptr =
	alloca $ \psize -> do
		pdata <- av_stream_get_side_data ptr (fromCEnum sdType) psize
		size <- fromIntegral <$> peek psize
		return (pdata, size)

class AVPacketSideDataPayload a => AVStreamSideDataPayload a where
	peekAVStreamSideDataPtr :: Ptr AVStream -> IO (Ptr a, Int)

instance AVStreamSideDataPayload AVPacketSideDataPalette where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_palette
instance AVStreamSideDataPayload AVPacketSideDataNewExtradata where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_new_extradata
instance AVStreamSideDataPayload AVPacketSideDataParamChange where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_param_change
instance AVStreamSideDataPayload AVPacketSideDataH263MbInfo where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_h263_mb_info
instance AVStreamSideDataPayload AVPacketSideDataReplayGain where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_replaygain
instance AVStreamSideDataPayload AVPacketSideDataDisplayMatrix where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_displaymatrix
instance AVStreamSideDataPayload AVPacketSideDataStereo3d where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_stereo3d
instance AVStreamSideDataPayload AVPacketSideDataSkipSamples where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_skip_samples
instance AVStreamSideDataPayload AVPacketSideDataJpDualmono where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_jp_dualmono
instance AVStreamSideDataPayload AVPacketSideDataStringsMetadata where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_strings_metadata
instance AVStreamSideDataPayload AVPacketSideDataSubtitlePosition where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_subtitle_position
instance AVStreamSideDataPayload AVPacketSideDataMatroskaBlockadditional where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_matroska_blockadditional
instance AVStreamSideDataPayload AVPacketSideDataWebvttIdentifier where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_webvtt_identifier
instance AVStreamSideDataPayload AVPacketSideDataWebvttSettings where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_webvtt_settings
instance AVStreamSideDataPayload AVPacketSideDataMetadataUpdate where
	peekAVStreamSideDataPtr = (first castPtr <$>).peekAVStreamSideDataPtrType av_pkt_data_metadata_update

-- | Get side data associated with an AVStream
streamGetSideData :: forall a m. (MonadIO m, AVStreamSideDataPayload a) =>
	AVStream -> m (Maybe (AVPacketSideData a))
streamGetSideData pkt = liftIO.withThis pkt$ \ptr -> do
	(pdata, dsize) <- peekAVStreamSideDataPtr ptr :: IO (Ptr a, Int)
	if (pdata == nullPtr) then return Nothing else do
		v <- peekPayload pdata (fromIntegral dsize)
		return.Just$ AVPacketSideData v

-- | Create a new program with the given id in an AVFormatContext
newProgram :: (MonadIO m, MonadError String m) => AVFormatContext -> Int -> m ()
newProgram ctx pid = do
	r <- liftIO.withThis ctx$ \pctx -> av_new_program pctx (fromIntegral pid)

	when (r == nullPtr)$
		throwError "newProgram: av_new_program returned a null pointer"

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
	if cid == av_codec_id_none then return Nothing else return$ Just cid

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
findDefaultStreamIndex ctx = liftIO.withThis ctx$ (fmap StreamIndex).av_find_default_stream_index

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
queryCodec :: MonadIO m => AVOutputFormat -> AVCodecID -> FFCompliance -> m (Maybe Bool)
queryCodec ouf cid compliance = do
	r <- liftIO.withThis ouf$ \po ->
		avformat_query_codec po (fromCEnum cid) (fromCEnum compliance)

	return$ case r of
		0 -> Just False
		1 -> Just True
		_ -> Nothing

-- | Guess the sample aspect ratio of a given frame within a given stream
-- within a given format context
guessSampleAspectRatio :: MonadIO m => AVFormatContext -> AVStream -> AVFrame -> m (Maybe Rational)
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
	
	if num == 0 && den == 1 then return Nothing else return.Just$ num % den

-- | Guess the frame rate at a given frame within a given stream within a given
-- format context
guessFrameRate :: MonadIO m => AVFormatContext -> AVStream -> AVFrame -> m (Maybe Rational)
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

	if num == 0 && den == 1 then return Nothing else return.Just$ num % den

