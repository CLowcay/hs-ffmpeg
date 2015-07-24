{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec.Core (
	AVCodecContext,
	AVCodec,
	AVCodecDescriptor,

	codecGetPktTimebase,
	codecSetPktTimebase,
	codecGetCodecDescriptor,
	codecSetCodecDescriptor,
	codecGetCodecProperties,
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

	getCodecContext,

	libAVCodecVersion
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Ratio
import Data.Version
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util.Dict

foreign import ccall "b_av_codec_get_pkt_timebase" av_codec_get_pkt_timebase :: Ptr AVCodecContext -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "b_av_codec_set_pkt_timebase" av_codec_set_pkt_timebase :: Ptr AVCodecContext -> CInt -> CInt -> IO ()
foreign import ccall "av_codec_get_codec_descriptor" av_codec_get_codec_descriptor :: Ptr AVCodecContext -> IO (Ptr AVCodecDescriptor)
foreign import ccall "av_codec_set_codec_descriptor" av_codec_set_codec_descriptor :: Ptr AVCodecContext -> Ptr AVCodecDescriptor -> IO ()
foreign import ccall "av_codec_get_codec_properties" av_codec_get_codec_properties :: Ptr AVCodecContext -> IO CUInt
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
-- foreign import ccall "avcodec_get_class" avcodec_get_class :: Ptr AVClass
-- foreign import ccall "avcodec_get_frame_class" avcodec_get_frame_class :: Ptr AVClass
-- foreign import ccall "avcodec_get_subtitle_rect_class" avcodec_get_subtitle_rect_class :: Ptr AVClass
foreign import ccall "avcodec_copy_context" avcodec_copy_context :: Ptr AVCodecContext -> Ptr AVCodecContext -> IO CInt
foreign import ccall "avcodec_open2" avcodec_open2 :: Ptr AVCodecContext -> Ptr AVCodec -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "avcodec_close" avcodec_close :: Ptr AVCodecContext -> IO CInt
-- foreign import ccall "avsubtitle_free" avsubtitle_free :: Ptr AVSubtitle -> IO ()

-- | AVCodecContext struct
newtype AVCodecContext = AVCodecContext (ForeignPtr (Ptr (AVCodecContext)))

instance ExternalPointer AVCodecContext where
	withThis (AVCodecContext ctx) io = withForeignPtr ctx (io . castPtr)

-- | AVCodec struct
newtype AVCodec = AVCodec (Ptr AVCodec)

instance ExternalPointer AVCodec where
	withThis (AVCodec ctx) io = io$ castPtr ctx

-- | Get the timebase of a packet
codecGetPktTimebase :: MonadIO m => AVCodecContext -> m Rational
codecGetPktTimebase ctx = liftIO.withThis ctx$ \ptr ->
	alloca $ \pnum ->
	alloca $ \pden -> do
		pctx <- peek ptr
		av_codec_get_pkt_timebase pctx pnum pden
		n <- fromIntegral<$> peek pnum
		d <- fromIntegral<$> peek pden
		return$ n % d

-- | Set the timebase of a packet
codecSetPktTimebase :: MonadIO m => AVCodecContext -> Rational -> m ()
codecSetPktTimebase ctx r = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	av_codec_set_pkt_timebase pctx
		(fromIntegral$ numerator r) (fromIntegral$ denominator r)

-- | AVCodecDescriptor struct
-- __WARNING__: The storable instance leaks memory when poking, so use sparingly
data AVCodecDescriptor = AVCodecDescriptor {
	avCodecDescriptor_id :: AVCodecId,
	avCodecDescriptor_type :: AVMediaType,
	avCodecDescriptor_name :: String,
	avCodecDescriptor_long_name :: String,
	avCodecDescriptor_props :: Int,
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
		long_name <- peekCString _long_name
		mime_types <- mapM peekCString =<< peekArray0 nullPtr _mime_types

		return$ AVCodecDescriptor
			_id _type name long_name (fromIntegral _props) mime_types
	poke ptr avcd = do
		name <- newCString (avCodecDescriptor_name avcd)
		long_name <- newCString (avCodecDescriptor_long_name avcd)
		mime_types <- newArray0 nullPtr =<< (mapM newCString$ avCodecDescriptor_mime_types avcd)
		#{poke AVCodecDescriptor, id} ptr (avCodecDescriptor_id avcd)
		#{poke AVCodecDescriptor, type} ptr (avCodecDescriptor_type avcd)
		#{poke AVCodecDescriptor, name} ptr name
		#{poke AVCodecDescriptor, long_name} ptr long_name
		#{poke AVCodecDescriptor, props} ptr (fromIntegral$ avCodecDescriptor_props avcd :: CInt)
		#{poke AVCodecDescriptor, mime_types} ptr mime_types

-- | Get information about a codec
codecGetCodecDescriptor :: MonadIO m => AVCodecContext -> m AVCodecDescriptor
codecGetCodecDescriptor ctx = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	peek =<< (av_codec_get_codec_descriptor pctx)

-- | Set the codec descriptor
-- __WARNING__: This function leaks memory so use sparingly
codecSetCodecDescriptor :: MonadIO m => AVCodecContext -> AVCodecDescriptor -> m ()
codecSetCodecDescriptor ctx d = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	pcd <- mallocBytes #{size AVCodecDescriptor}
	poke pcd d
	av_codec_set_codec_descriptor pctx pcd

-- | Get the properties flags from a codec context
codecGetCodecProperties :: MonadIO m => AVCodecContext -> m AVCodecProp
codecGetCodecProperties ctx = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	toCEnum.fromIntegral <$> av_codec_get_codec_properties pctx

-- | Get the lowres field from a codec context
codecGetLowres :: MonadIO m => AVCodecContext -> m Int
codecGetLowres ctx = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	fromIntegral <$> av_codec_get_lowres pctx

-- | Set the lowres field in a codec context
codecSetLowres :: MonadIO m => AVCodecContext -> Int -> m ()
codecSetLowres ctx v = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	av_codec_set_lowres pctx (fromIntegral v)

-- | Get the seek preroll value from a codec context
codecGetSeekPreroll :: MonadIO m => AVCodecContext -> m Int
codecGetSeekPreroll ctx = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	fromIntegral <$> av_codec_get_seek_preroll pctx

-- | Set the seek preroll value in a codec context
codecSetSeekPreroll :: MonadIO m => AVCodecContext -> Int -> m ()
codecSetSeekPreroll ctx v = liftIO.withThis ctx$ \ptr -> do
	pctx <- peek ptr
	av_codec_set_seek_preroll pctx (fromIntegral v)

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
	
--foreign import ccall "avcodec_get_class" avcodec_get_class :: Ptr AVClass
--foreign import ccall "avcodec_get_frame_class" avcodec_get_frame_class :: Ptr AVClass
--foreign import ccall "avcodec_get_subtitle_rect_class" avcodec_get_subtitle_rect_class :: Ptr AVClass

-- | Which version of libavcodec are we using?
libAVCodecVersion :: Version
libAVCodecVersion = fromVersionNum #{const LIBAVCODEC_VERSION_INT}

