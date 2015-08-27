{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Core (
	avsubtitle_free,

	AVCodecContext(..),
	AVCodec(..),
	AVCodecDescriptor(..),

	AVSubtitle(..),
	AVSubtitleRect(..),

	codecGetPktTimebase,
	codecSetPktTimebase,
	codecGetCodecDescriptor,
	codecSetCodecDescriptor,
	--codecGetCodecProperties,
	codecGetLowres,
	codecSetLowres,
	codecGetSeekPreroll,
	codecSetSeekPreroll,
	codecGetMaxLowres,
	getRegisteredCodecs,
	avcodecConfiguration,
	avcodecLicense,
	avcodecRegister,
	avcodecRegisterAll,

	copyCodecContext,
	getCodecContext,

	libAVCodecVersion,

	getCodecTagString,
	codecString,
	getProfileName,
	flushBuffers,
	getBitsPerSample,
	Endianness(..),
	getPCMCodec,
	getExactBitsPerSample,
	codecGetType,
	codecGetName,
	codecIsEncoder,
	codecIsDecoder,
	codecDescriptorGet,
	codecDescriptors,
	codecDescriptorGetByName
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Ratio
import Data.Traversable hiding (mapM)
import Data.Version
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

import Media.FFMpeg.Codec.AVPicture
import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "b_av_codec_get_pkt_timebase" av_codec_get_pkt_timebase :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "b_av_codec_set_pkt_timebase" av_codec_set_pkt_timebase :: Ptr AVCodecContext -> CInt -> CInt -> IO ()
foreign import ccall "av_codec_get_codec_descriptor" av_codec_get_codec_descriptor :: Ptr AVCodecContext -> IO (Ptr AVCodecDescriptor)
foreign import ccall "av_codec_set_codec_descriptor" av_codec_set_codec_descriptor :: Ptr AVCodecContext -> Ptr AVCodecDescriptor -> IO ()
--foreign import ccall "av_codec_get_codec_properties" av_codec_get_codec_properties :: Ptr AVCodecContext -> IO CUInt
foreign import ccall "av_codec_get_lowres" av_codec_get_lowres :: Ptr AVCodecContext -> IO CInt
foreign import ccall "av_codec_set_lowres" av_codec_set_lowres :: Ptr AVCodecContext -> CInt -> IO ()
foreign import ccall "av_codec_get_seek_preroll" av_codec_get_seek_preroll :: Ptr AVCodecContext -> IO CInt
foreign import ccall "av_codec_set_seek_preroll" av_codec_set_seek_preroll :: Ptr AVCodecContext -> CInt -> IO ()
foreign import ccall "av_codec_get_chroma_intra_matrix" av_codec_get_chroma_intra_matrix :: Ptr AVCodecContext -> IO (Ptr Word16)
foreign import ccall "av_codec_set_chroma_intra_matrix" av_codec_set_chroma_intra_matrix :: Ptr AVCodecContext -> Ptr Word16 -> IO ()
foreign import ccall "av_codec_get_max_lowres" av_codec_get_max_lowres :: Ptr AVCodec -> IO CInt
foreign import ccall "av_codec_next" av_codec_next :: Ptr AVCodec -> IO (Ptr AVCodec)
foreign import ccall "avcodec_version" avcodec_version :: IO CUInt
foreign import ccall "avcodec_configuration" avcodec_configuration :: CString
foreign import ccall "avcodec_license" avcodec_license :: CString
foreign import ccall "avcodec_register" avcodec_register :: Ptr AVCodec -> IO ()
foreign import ccall "avcodec_register_all" avcodec_register_all :: IO ()
foreign import ccall "avcodec_alloc_context3" avcodec_alloc_context3 :: Ptr AVCodec -> IO (Ptr AVCodecContext)
foreign import ccall "avcodec_free_context" avcodec_free_context :: Ptr (Ptr AVCodecContext) -> IO ()
foreign import ccall "&avcodec_free_context" pavcodec_free_context :: FunPtr (Ptr (Ptr AVCodecContext) -> IO ())
foreign import ccall "avcodec_get_context_defaults3" avcodec_get_context_defaults3 :: Ptr AVCodecContext -> Ptr AVCodec -> IO CInt
foreign import ccall "avcodec_get_class" avcodec_get_class :: Ptr (AVClass AVCodecContext)
foreign import ccall "avcodec_get_subtitle_rect_class" avcodec_get_subtitle_rect_class :: Ptr (AVClass AVSubtitleRect)
foreign import ccall "avcodec_copy_context" avcodec_copy_context :: Ptr AVCodecContext -> Ptr AVCodecContext -> IO CInt
foreign import ccall "avcodec_open2" avcodec_open2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr (UnderlyingType AVDictionary) -> IO CInt
foreign import ccall "avcodec_close" avcodec_close :: Ptr AVCodecContext -> IO CInt
foreign import ccall "avsubtitle_free" avsubtitle_free :: Ptr () -> IO ()
foreign import ccall "&avsubtitle_free" pavsubtitle_free :: FunPtr (Ptr () -> IO ())

foreign import ccall "av_get_codec_tag_string" av_get_codec_tag_string :: CString -> CInt -> CUInt -> IO CSize
foreign import ccall "avcodec_string" avcodec_string :: CString -> CInt -> Ptr AVCodecContext -> CInt -> IO ()
foreign import ccall "av_get_profile_name" av_get_profile_name :: Ptr AVCodec -> CInt -> IO CString
foreign import ccall "avcodec_fill_audio_frame" avcodec_fill_audio_frame :: Ptr AVFrame -> CInt -> CInt -> Ptr Word8 -> CInt -> CInt -> IO CInt
foreign import ccall "avcodec_flush_buffers" avcodec_flush_buffers :: Ptr AVCodecContext -> IO ()
foreign import ccall "av_get_bits_per_sample" av_get_bits_per_sample :: CInt -> CInt
foreign import ccall "av_get_pcm_codec" av_get_pcm_codec :: CInt -> CInt -> CInt
foreign import ccall "av_get_exact_bits_per_sample" av_get_exact_bits_per_sample :: CInt -> CInt
foreign import ccall "av_get_audio_frame_duration" av_get_audio_frame_duration :: Ptr AVCodecContext -> CInt -> IO CInt
foreign import ccall "av_fast_padded_malloc" av_fast_padded_malloc :: Ptr () -> Ptr CUInt -> CSize -> IO ()
foreign import ccall "av_fast_padded_mallocz" av_fast_padded_mallocz :: Ptr () -> Ptr CUInt -> CSize -> IO ()
foreign import ccall "av_xiphlacing" av_xiphlacing :: CString -> CUInt -> IO CUInt
foreign import ccall "av_lockmgr_register" av_lockmgr_register :: FunPtr (Ptr (Ptr ()) -> CInt -> IO CInt) -> IO CInt
foreign import ccall "avcodec_get_type" avcodec_get_type :: CInt -> CInt
foreign import ccall "avcodec_get_name" avcodec_get_name :: CInt -> CString
foreign import ccall "avcodec_is_open" avcodec_is_open :: Ptr AVCodecContext -> IO CInt
foreign import ccall "av_codec_is_encoder" av_codec_is_encoder :: Ptr AVCodec -> IO CInt
foreign import ccall "av_codec_is_decoder" av_codec_is_decoder :: Ptr AVCodec -> IO CInt
foreign import ccall "avcodec_descriptor_get" avcodec_descriptor_get :: CInt -> Ptr ()
foreign import ccall "avcodec_descriptor_next" avcodec_descriptor_next :: Ptr () -> Ptr ()
foreign import ccall "avcodec_descriptor_get_by_name" avcodec_descriptor_get_by_name :: CString -> Ptr ()

-- | AVCodecContext struct
newtype AVCodecContext = AVCodecContext (ForeignPtr (Ptr (AVCodecContext)))
instance ExternalPointer AVCodecContext where
	type UnderlyingType AVCodecContext = AVCodecContext
	withThis (AVCodecContext fp) action = withThis fp$ \p -> action =<< (liftIO$ peek p)
instance ReflectClass AVCodecContext where
	withClass ctx action = withThis ctx$ \pctx ->
		action =<< (liftIO$ avClassFromPtr <$> #{peek AVCodecContext, av_class} pctx)
instance HasClass AVCodecContext where
	getClass = avClassFromPtr avcodec_get_class

-- | AVCodec struct
newtype AVCodec = AVCodec (Ptr AVCodec) deriving (Storable) -- Ptr is OK because codecs are never freed
instance ExternalPointer AVCodec where
	type UnderlyingType AVCodec = AVCodec
	withThis (AVCodec p) = withThis p

-- | AVSubtitle struct
data AVSubtitle = AVSubtitle {
		avSubtitle_format :: Word16,
		avSubtitle_start_display_time :: Word32,
		avSubtitle_end_display_time :: Word32,
		avSubtitle_rects :: [AVSubtitleRect],
		avSubtitle_pts :: AVTimestamp
	}

-- | AVSubtitleRect struct
data AVSubtitleRect = forall avpicture. HasAVPicture avpicture =>
	AVSubtitleRect {
		avSubtitleRect_x :: Int,
		avSubtitleRect_y :: Int,
		avSubtitleRect_w :: Int,
		avSubtitleRect_h :: Int,
		avSubtitleRect_nb_colors :: Int,
		avSubtitleRect_pict :: avpicture,
		avSubtitleRect_type :: AVSubtitleType,
		avSubtitleRect_text :: Maybe String,
		avSubtitleRect_ass :: Maybe String,
		avSubtitleRect_flags :: AVSubtitleFlag
	}

instance HasClass AVSubtitleRect where
	getClass = avClassFromPtr avcodec_get_subtitle_rect_class

instance Storable AVSubtitleRect where
	sizeOf _ = #{size AVSubtitleRect}
	alignment _ = 8
	peek ptr = do
		_x <- #{peek AVSubtitleRect, x} ptr :: IO CInt
		_y <- #{peek AVSubtitleRect, y} ptr :: IO CInt
		_w <- #{peek AVSubtitleRect, w} ptr :: IO CInt
		_h <- #{peek AVSubtitleRect, h} ptr :: IO CInt
		_nb_colors <- #{peek AVSubtitleRect, nb_colors} ptr :: IO CInt
		_type <- #{peek AVSubtitleRect, type} ptr
		_text <- #{peek AVSubtitleRect, text} ptr
		_ass <- #{peek AVSubtitleRect, ass} ptr
		_flags <- #{peek AVSubtitleRect, flags} ptr

		text <- peekCString `traverse` justPtr _text
		ass <- peekCString `traverse` justPtr _ass

		fpSub <- newForeignPtr_ (castPtr ptr)
		addForeignPtrFinalizer pav_free fpSub
		addForeignPtrFinalizer pavsubtitle_free fpSub
		let pict = AVSubtitlePicture fpSub #{offset AVSubtitleRect, pict}

		return$ AVSubtitleRect
			(fromIntegral _x) (fromIntegral _y)
			(fromIntegral _w) (fromIntegral _h)
			(fromIntegral _nb_colors)
			pict _type text ass _flags

	poke ptr avsr = do
		pText <- case avSubtitleRect_text avsr of
			Nothing -> return nullPtr
			Just p -> withCStringLen p newAVCString

		pASS <- case avSubtitleRect_ass avsr of
			Nothing -> return nullPtr
			Just p -> withCStringLen p newAVCString

		#{poke AVSubtitleRect, x} ptr (toCInt$ avSubtitleRect_x avsr)
		#{poke AVSubtitleRect, y} ptr (toCInt$ avSubtitleRect_y avsr)
		#{poke AVSubtitleRect, w} ptr (toCInt$ avSubtitleRect_w avsr)
		#{poke AVSubtitleRect, h} ptr (toCInt$ avSubtitleRect_h avsr)
		#{poke AVSubtitleRect, nb_colors} ptr (toCInt$ avSubtitleRect_nb_colors avsr)

		case avsr of
			AVSubtitleRect {avSubtitleRect_pict = pict} -> withAVPicturePtr pict$ \ppict ->
				moveBytes (ptr `plusPtr` #{offset AVSubtitleRect, pict}) ppict #{size AVPicture}

		#{poke AVSubtitleRect, type} ptr (fromCEnum$ avSubtitleRect_type avsr)
		#{poke AVSubtitleRect, text} ptr pText
		#{poke AVSubtitleRect, ass} ptr pASS
		#{poke AVSubtitleRect, flags} ptr (fromCEnum$ avSubtitleRect_flags avsr)

		where
			newAVCString :: (CString, Int) -> IO CString
			newAVCString (p, l) = do
				pa <- castPtr <$> (av_malloc$ fromIntegral (l+1))
				copyArray pa p l
				poke (pa `plusPtr` l) (0 :: CChar)
				return pa
			toCInt :: Integral a => a -> CInt
			toCInt = fromIntegral

-- | __WARNING__: Do not call avsubtitle_free on a peeked AVSubtitle unless
-- there are no rects
instance Storable AVSubtitle where
	sizeOf _ = #{size AVSubtitle}
	alignment _ = 8
	peek ptr = do
		_format <- #{peek AVSubtitle, format} ptr
		_start_display_time <- #{peek AVSubtitle, start_display_time} ptr
		_end_display_time <- #{peek AVSubtitle, end_display_time} ptr
		num_rects <- #{peek AVSubtitle, num_rects} ptr :: IO CUInt
		_rects <- peekArray (fromIntegral num_rects)$ ptr `plusPtr` #{offset AVSubtitle, rects}
		_pts <- #{peek AVSubtitle, pts} ptr

		return$ AVSubtitle
			_format _start_display_time
			_end_display_time _rects _pts

	poke ptr avs = do
		let rects = avSubtitle_rects avs
		prect <- av_malloc$
			(fromIntegral$ sizeOf (undefined :: AVSubtitleRect)) *
			(fromIntegral$ length rects)
		pokeArray (castPtr prect) rects
		
		#{poke AVSubtitle, format} ptr (avSubtitle_format avs)
		#{poke AVSubtitle, start_display_time} ptr (avSubtitle_start_display_time avs)
		#{poke AVSubtitle, end_display_time} ptr (avSubtitle_end_display_time avs)
		#{poke AVSubtitle, num_rects} ptr (length$ avSubtitle_rects avs)
		#{poke AVSubtitle, rects} ptr prect
		#{poke AVSubtitle, pts} ptr (avSubtitle_pts avs)

-- | Get the timebase of a packet
codecGetPktTimebase :: MonadIO m => AVCodecContext -> m AVRational
codecGetPktTimebase ctx =
	withThis ctx$ \pctx ->
	liftIO$
		alloca$ \pnum ->
		alloca$ \pden -> do
			av_codec_get_pkt_timebase pctx pnum pden
			n <- fromIntegral<$> peek pnum
			d <- fromIntegral<$> peek pden
			return.AVRational$ n % d

-- | Set the timebase of a packet
codecSetPktTimebase :: MonadIO m => AVCodecContext -> AVRational -> m ()
codecSetPktTimebase ctx (AVRational r) = withThis ctx$ \pctx ->
	liftIO$ av_codec_set_pkt_timebase pctx
		(fromIntegral$ numerator r) (fromIntegral$ denominator r)

-- | AVCodecDescriptor struct
-- __WARNING__: The storable instance leaks memory when poking, so use sparingly
data AVCodecDescriptor = AVCodecDescriptor {
	avCodecDescriptor_id :: AVCodecID,
	avCodecDescriptor_type :: AVMediaType,
	avCodecDescriptor_name :: String,
	avCodecDescriptor_long_name :: Maybe String,
	avCodecDescriptor_props :: AVCodecProp,
	avCodecDescriptor_mime_types :: [String]
}

instance Storable AVCodecDescriptor where
	sizeOf _ = #{size AVCodecDescriptor}
	alignment _ = 8
	peek ptr = do
		_id <- #{peek AVCodecDescriptor, id} ptr
		_type <- #{peek AVCodecDescriptor, type} ptr
		_name <- #{peek AVCodecDescriptor, name} ptr :: IO CString
		_long_name <- #{peek AVCodecDescriptor, long_name} ptr :: IO CString
		_props <- #{peek AVCodecDescriptor, props} ptr :: IO CInt
		_mime_types <- #{peek AVCodecDescriptor, mime_types} ptr :: IO (Ptr CString)
	
		name <- peekCString _name
		long_name <- if _long_name == nullPtr then return Nothing
			else Just <$> peekCString _long_name
		mime_types <- if _mime_types == nullPtr then return []
			else mapM peekCString =<< peekArray0 nullPtr _mime_types

		return$ AVCodecDescriptor
			_id _type name long_name (toCEnum _props) mime_types
	poke ptr avcd = do
		name <- newCString (avCodecDescriptor_name avcd)
		long_name <- case avCodecDescriptor_long_name avcd of
			Nothing -> return nullPtr
			Just s -> newCString s
		mime_types <- newArray0 nullPtr =<< (mapM newCString$ avCodecDescriptor_mime_types avcd)
		#{poke AVCodecDescriptor, id} ptr (avCodecDescriptor_id avcd)
		#{poke AVCodecDescriptor, type} ptr (avCodecDescriptor_type avcd)
		#{poke AVCodecDescriptor, name} ptr name
		#{poke AVCodecDescriptor, long_name} ptr long_name
		#{poke AVCodecDescriptor, props} ptr (fromCEnum$ avCodecDescriptor_props avcd)
		#{poke AVCodecDescriptor, mime_types} ptr mime_types

-- | Get information about a codec
codecGetCodecDescriptor :: MonadIO m => AVCodecContext -> m AVCodecDescriptor
codecGetCodecDescriptor ctx = withThis ctx$ \pctx ->
	liftIO$ peek =<< (av_codec_get_codec_descriptor pctx)

-- | Set the codec descriptor
-- __WARNING__: This function leaks memory so use sparingly
codecSetCodecDescriptor :: MonadIO m => AVCodecContext -> AVCodecDescriptor -> m ()
codecSetCodecDescriptor ctx d = withThis ctx$ \pctx -> liftIO$ do
	pcd <- mallocBytes #{size AVCodecDescriptor}
	poke pcd d
	av_codec_set_codec_descriptor pctx pcd

-- | Get the properties flags from a codec context
-- codecGetCodecProperties :: MonadIO m => AVCodecContext -> m AVCodecProp
-- codecGetCodecProperties ctx = withThis ctx$ \pctx -> liftIO$
-- 	toCEnum.fromIntegral <$> av_codec_get_codec_properties pctx

-- | Get the lowres field from a codec context
codecGetLowres :: MonadIO m => AVCodecContext -> m Int
codecGetLowres ctx = withThis ctx$ \pctx -> liftIO$
	fromIntegral <$> av_codec_get_lowres pctx

-- | Set the lowres field in a codec context
codecSetLowres :: MonadIO m => AVCodecContext -> Int -> m ()
codecSetLowres ctx v = withThis ctx$ \pctx -> liftIO$
	av_codec_set_lowres pctx (fromIntegral v)

-- | Get the seek preroll value from a codec context
codecGetSeekPreroll :: MonadIO m => AVCodecContext -> m Int
codecGetSeekPreroll ctx = withThis ctx$ \pctx -> do
	liftIO$ fromIntegral <$> av_codec_get_seek_preroll pctx

-- | Set the seek preroll value in a codec context
codecSetSeekPreroll :: MonadIO m => AVCodecContext -> Int -> m ()
codecSetSeekPreroll ctx v = withThis ctx$ \pctx ->
	liftIO$ av_codec_set_seek_preroll pctx (fromIntegral v)

-- foreign import ccall "av_codec_get_chroma_intra_matrix" av_codec_get_chroma_intra_matrix :: Ptr AVCodecContext -> IO (Ptr Word16)
-- foreign import ccall "av_codec_set_chroma_intra_matrix" av_codec_set_chroma_intra_matrix :: Ptr AVCodecContext -> Ptr Word16 -> IO ()

-- | Get the maximum lowres value for a codec
codecGetMaxLowres :: MonadIO m => AVCodec -> m Int
codecGetMaxLowres cd = liftIO.withThis cd$ \ptr ->
	fromIntegral <$> av_codec_get_max_lowres ptr

-- | Get all the codecs that have been registered
getRegisteredCodecs :: MonadIO m => m [AVCodec]
getRegisteredCodecs = liftIO$ allCodecs nullPtr
	where
		allCodecs :: Ptr AVCodec -> IO [AVCodec]
		allCodecs prev = do
			r <- av_codec_next prev
			if r == nullPtr then return [] else ((AVCodec r) :) <$> allCodecs r

-- | Get the libav_codec configuration string
avcodecConfiguration :: String
avcodecConfiguration =
	-- safe because avcodec_configuration returns a const string
	unsafePerformIO$ peekCString avcodec_configuration

-- | Get the libav_codec license string
avcodecLicense :: String
avcodecLicense =
	-- safe because avcodec_license returns a const string
	unsafePerformIO$ peekCString avcodec_license

-- | Register a single codec
avcodecRegister :: MonadIO m => AVCodec -> m ()
avcodecRegister cd = liftIO.withThis cd$ \ptr -> avcodec_register ptr

-- | Register all the codecs
avcodecRegisterAll :: MonadIO m => m ()
avcodecRegisterAll = liftIO avcodec_register_all

-- | Copy all the data from one codec context to another
copyCodecContext :: (MonadIO m, MonadError String m) =>
	AVCodecContext     -- ^ Destination context
	-> AVCodecContext  -- ^ Source context
	-> m ()
copyCodecContext dst src =
	withThis dst$ \pdst ->
	withThis src$ \psrc -> do
		r <- liftIO$ avcodec_copy_context pdst psrc
		when (r /= 0)$ throwError$ "copyCodecContext: failed with error code " ++ (show r)

-- | Allocate a new codec context
getCodecContext :: (MonadIO m, MonadError String m) => AVCodec -> AVDictionary -> m AVCodecContext
getCodecContext cd dict = do
	pctx <- liftIO$ withThis cd avcodec_alloc_context3

	when (pctx == nullPtr)$
		throwError$ "getCodecContext: avcodec_alloc_context3 returned a null pointer"

	r <- liftIO$
		withThis cd$ \pCodec ->
		withThis dict$ \ppdict ->
			avcodec_open2 pctx pCodec ppdict

	when (r /= 0)$
		throwError$ "getCodecContext: avcodec_open2 failed with error code " ++ (show r)

	liftIO$ do
		fpctx <- mallocForeignPtr
		withForeignPtr fpctx $ \ppctx -> poke ppctx pctx
		addForeignPtrFinalizer pavcodec_free_context fpctx
		return$ AVCodecContext fpctx
	
-- | Which version of libavcodec are we using?
libAVCodecVersion :: Version
libAVCodecVersion = fromVersionNum #{const LIBAVCODEC_VERSION_INT}

-- | Convert a codec tag to a string
getCodecTagString :: Word -> String
getCodecTagString tag = unsafePerformIO$ do  -- safe because there are no observable side effects
	sz <- av_get_codec_tag_string nullPtr 0 (fromIntegral tag)
	allocaBytes (fromIntegral sz)$ \pb -> do
		av_get_codec_tag_string pb (fromIntegral sz) (fromIntegral tag)
		peekCStringLen (pb, fromIntegral sz)

-- | Get information about a codec in a string
codecString :: MonadIO m => AVCodecContext -> Bool -> m String
codecString ctx isEncoder =
	withThis ctx$ \pctx ->
	liftIO.allocaBytes buffsize$ \pb -> do
		avcodec_string pb (fromIntegral buffsize) pctx (if isEncoder then 1 else 0)
		peekCString pb
	where buffsize = 4096

-- | Get the string name of a profile ID
getProfileName :: MonadIO m => AVCodec -> FFProfile -> m String
getProfileName cd profile = liftIO.withThis cd$ \pcd ->
	peekCString =<< av_get_profile_name pcd (fromCEnum profile)

-- | Flush buffers associated with a codec and reset it to its default state
flushBuffers :: MonadIO m => AVCodecContext -> m ()
flushBuffers ctx = withThis ctx$ \pctx -> liftIO$ avcodec_flush_buffers pctx

-- | See libav docs
getBitsPerSample :: AVCodecID -> Int
getBitsPerSample = fromIntegral.av_get_bits_per_sample.fromCEnum

data Endianness = LittleEndian | BigEndian | NativeEndian deriving (Eq, Show, Enum)

-- | Get a PCM codec for the given sample format and endianness
getPCMCodec :: AVSampleFormat -> Endianness -> AVCodecID
getPCMCodec format end = toCEnum$ av_get_pcm_codec (fromCEnum format) (encodeEndianness end)
	where
		encodeEndianness LittleEndian = 0
		encodeEndianness BigEndian = 1
		encodeEndianness NativeEndian = -1

-- | See libav docs
getExactBitsPerSample :: AVCodecID -> Int
getExactBitsPerSample = fromIntegral.av_get_exact_bits_per_sample.fromCEnum

-- | Get the media type of a codec
codecGetType :: AVCodecID -> AVMediaType
codecGetType = toCEnum.avcodec_get_type.fromCEnum

-- | Get the string name of a codec
codecGetName :: AVCodecID -> String
codecGetName =
	-- safe because it's a const string
	unsafePerformIO.peekCString.avcodec_get_name.fromCEnum

-- | Determine if the codec is an encoder
codecIsEncoder :: MonadIO m => AVCodec -> m Bool
codecIsEncoder cd = liftIO.withThis cd$ \pcd -> (/= 0) <$> av_codec_is_encoder pcd

-- | Determine if the codec is a decoder
codecIsDecoder :: MonadIO m => AVCodec -> m Bool
codecIsDecoder cd = liftIO.withThis cd$ \pcd -> (/= 0) <$> av_codec_is_decoder pcd

-- | Get the descriptor of a codec
codecDescriptorGet :: AVCodecID -> AVCodecDescriptor
codecDescriptorGet =
	-- safe because avcodec_descriptor_get returns a const *
	unsafePerformIO.peek.castPtr.avcodec_descriptor_get.fromCEnum

-- | Descriptors for all the codecs known to libav
codecDescriptors :: [AVCodecDescriptor]
codecDescriptors =
	-- safe because avcodec_descriptor_next returns const *
	unsafePerformIO$ mapM peek$
		takeWhile (/= nullPtr)$
			castPtr <$> iterate avcodec_descriptor_next
				(avcodec_descriptor_next nullPtr)

-- | Get the descriptor of a codec by name
codecDescriptorGetByName :: String -> Maybe AVCodecDescriptor
codecDescriptorGetByName s =
	-- safe because avcodec_descriptor_get_by_name returns a const *
	unsafePerformIO.withCString s$
		(peek `traverse`).justPtr.castPtr.avcodec_descriptor_get_by_name 

