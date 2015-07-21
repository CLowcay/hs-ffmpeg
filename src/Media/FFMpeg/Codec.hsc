-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec (
	module Media.FFMpeg.Codec.Enums,

	libAVCodecVersion,

	AVCodecContext,

	AVCodec,
	openCodec,
	findDecoder,
	decodeVideo2,
	decodeAudio3,

	AVPacket,
	allocPacket,
	newPacket,
	packetGetStreamIndex,
	packetGetSize
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.ByteString (ByteString, append, packCStringLen)
import Data.Int
import Data.Version
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Error
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Text.Printf

import Media.FFMpeg.Codec.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Util.AVFrame

foreign import ccall "avcodec_open" avcodec_open :: Ptr () -> Ptr () -> IO CInt
-- TODO: add avcodec_open2
foreign import ccall "&avcodec_close" pavcodec_close :: FunPtr (Ptr () -> IO ())
foreign import ccall "avcodec_find_decoder" avcodec_find_decoder :: 
	CInt -> IO (Ptr ())
foreign import ccall "avcodec_decode_video2" avcodec_decode_video2 :: 
	Ptr () -> Ptr () -> Ptr CInt -> Ptr () -> IO CInt
foreign import ccall "avcodec_decode_audio3" avcodec_decode_audio3 :: 
	Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> IO CInt
-- TODO: add avcodec_decode_audio4
-- These functions go through a wrapper due to inlining
foreign import ccall "b_init_packet" av_init_packet :: Ptr () -> IO ()
foreign import ccall "&b_free_packet" pav_free_packet ::
	FunPtr (Ptr () -> IO ())
foreign import ccall "av_new_packet" av_new_packet :: Ptr () -> CInt -> IO CInt
foreign import ccall "av_frame_get_buffer" av_frame_get_buffer ::
	Ptr () -> CInt -> IO CInt


-- | Which version of libavcodec are we using?
libAVCodecVersion :: Version
libAVCodecVersion = fromVersionNum #{const LIBAVCODEC_VERSION_INT}

-- | For structs that have a codecID field
class HasCodecId a where
	getCodecId :: a -> IO AVCodecId

-- | For structs that have a codecType field
class HasCodecType a where
	getCodecType :: a -> IO AVMediaType

-- | AVCodecContext struct
newtype AVCodecContext = AVCodecContext (ForeignPtr AVCodecContext)

instance ExternalPointer AVCodecContext where
	withThis (AVCodecContext ctx) io = withForeignPtr ctx (io . castPtr)

instance HasCodecId AVCodecContext where
	getCodecId ctx = withThis ctx $ \ctx' ->
		toCEnum <$> (#{peek AVCodecContext, codec_id} ctx' :: IO CInt)

instance HasCodecType AVCodecContext where
	getCodecType ctx = withThis ctx $ \ctx' ->
		toCEnum <$> (#{peek AVCodecContext, codec_type} ctx' :: IO CInt)

getVideoWidth :: AVCodecContext -> IO Int
getVideoWidth ctx = withThis ctx $ \ctx' ->
	fromIntegral <$> (#{peek AVCodecContext, width} ctx' :: IO CInt)

getVideoHeight :: AVCodecContext -> IO Int
getVideoHeight ctx = withThis ctx $ \ctx' -> 
	fromIntegral <$> (#{peek AVCodecContext, height} ctx' :: IO CInt)

getPixelFormat :: AVCodecContext -> IO PixelFormat
getPixelFormat ctx = withThis ctx $ \ctx' -> 
	toCEnum <$> (#{peek AVCodecContext, pix_fmt} ctx' :: IO CInt)

getAudioSampleRate :: AVCodecContext -> IO Int
getAudioSampleRate ctx = withThis ctx $ \ctx' ->
	fromIntegral <$> (#{peek AVCodecContext, sample_rate} ctx' :: IO CInt)

getAudioChannels :: AVCodecContext -> IO Int
getAudioChannels ctx = withThis ctx $ \ctx' ->
	fromIntegral <$> (#{peek AVCodecContext, channels} ctx' :: IO CInt)

getAudioSampleFormat :: AVCodecContext -> IO AVSampleFormat
getAudioSampleFormat ctx = withThis ctx $ \ctx' ->
	toCEnum <$> (#{peek AVCodecContext, sample_fmt} ctx' :: IO CInt)

-- | AVCodec struct
newtype AVCodec = AVCodec (Ptr AVCodec)

instance ExternalPointer AVCodec where
	withThis (AVCodec c) io = io (castPtr c)

instance HasCodecId AVCodec where
	getCodecId c = withThis c $ \c' ->
		toCEnum <$> (#{peek AVCodec, id} c' :: IO CInt)

instance HasCodecType AVCodec where
	getCodecType c = withThis c $ \c' ->
		toCEnum <$> (#{peek AVCodec, type} c' :: IO CInt)

-- | Initialize an AVCodecContext to use the given AVCodec.
-- __Warning:__ This function is not thread safe
openCodec :: (MonadIO m, MonadError String m) =>
	AVCodecContext  -- ^ AVCodecContext to initialize
	-> AVCodec      -- ^ AVCodec to use
	-> m ()
openCodec ctx@(AVCodecContext ct) codec = do
	r <- liftIO$
		withThis ctx $ \ctx' ->
		withThis codec $ \codec' -> avcodec_open ctx' codec'

	if (r < 0) then do
		codecString <- liftIO (show <$> getCodecId codec)
		throwError$ "openCodec: failed to open codec " ++ codecString ++ ", error " ++ (show r)
	else do
		liftIO$ addForeignPtrFinalizer pavcodec_close (castForeignPtr ct)

-- | Find a registered decoder with a matching codec ID
findDecoder :: (MonadIO m, MonadError String m) => AVCodecId -> m (Maybe AVCodec)
findDecoder cid = liftIO$
	fmap (AVCodec . castPtr) . justPtr <$> avcodec_find_decoder (fromCEnum cid)

-- | Decode a video frame into a picture
-- __Warning:__ The input buffer must be FF_INPUT_BUFFER_PADDING_SIZE larger
-- than the number of bytes that will be read.
decodeVideo2 :: (MonadIO m, MonadError String m) =>
	AVCodecContext   -- ^ the Codec context
	-> AVFrame       -- ^ the frame in which the output will be stored
	-> AVPacket      -- ^ the input buffer
	-> m Bool        -- ^ False if no frame could be decoded, otherwise True
decodeVideo2 ctx frm pkt = do
	pGotPic <- liftIO malloc
	r <- liftIO$
		withThis ctx $ \ctx' -> 
		withThis frm $ \frm' -> 
		withThis pkt $ \pkt' ->
			avcodec_decode_video2 ctx' frm' pGotPic pkt'

	if r < 0 then do
		liftIO$ free pGotPic
		throwError$ "decodeVideo2: failed to decode video packet, error " ++ (show r)
	else liftIO$ do
		r <- peek pGotPic
		free pGotPic
		return$ r /= 0

-- | Decode an audio frame into samples
decodeAudio3 :: (MonadIO m, MonadError String m) =>
	AVCodecContext    -- ^ the codec context
	-> Buffer         -- ^ the output buffer
	-> AVPacket       -- ^ the input packet containing the input buffer
	-> m (Int, Int)   -- ^ (number of bytes used, output buffer size)
decodeAudio3 ctx buf pkt = do
	pfrm_size <- liftIO malloc
	liftIO$ poke pfrm_size bufferCSize
	
	r <- liftIO$
		withThis ctx $ \ctx' -> 
		withThis buf $ \buf' ->
		withThis pkt $ \pkt' -> do
			fromIntegral <$> avcodec_decode_audio3 ctx' buf' pfrm_size pkt'

	if (r < 0) then do
		liftIO$ free pfrm_size
		throwError$ "decodeAudio3: decode packet failed, error " ++ (show r)
	else liftIO$ do
		r2 <- ((r, ) . fromIntegral) <$> peek pfrm_size
		free pfrm_size
		return r2

	where bufferCSize = fromIntegral $ bufferSize buf :: CInt

--
-- | decodeAudio is very complex to use, the another version of the 
-- same function is decode audio packet. It decodes packets into a list of
-- buffers with max size is set to 'size'. 
--

-- decodeAudioPacket :: Int -> AVCodecContext -> AVPacket -> IO ByteString
-- decodeAudioPacket buffSize ctx pkt = dupPacket pkt >> fillBuffer 
--     where 
--       shiftPacket size = packetSetBuffer pkt =<< 
--                          shiftBuffer size (packetGetBuffer pkt)
--       copyToByteString buf size = 
--           withBuffer buf $ \buf' ->
--               packCStringLen (castPtr buf', size)
-- 
--       fillBuffer = do
--         buf <- allocBuffer buffSize
--         (r, w) <- decodeAudio3 ctx buf pkt
--         result <- copyToByteString buf w
--         if r < (packetGetSize pkt) then do
--                     shiftPacket r
--                     rest <- fillBuffer
--                     return $  result `append` rest
--                   else return $ result

-- TODO: review these functions
      
--maxAudioFrameSize :: Int
--maxAudioFrameSize = #{const AVCODEC_MAX_AUDIO_FRAME_SIZE}

--
-- | decodeAudioPacket' - defaults the buffer size to 2048
--
--decodeAudioPacket' :: CodecContext -> Packet -> IO ByteString
--decodeAudioPacket' = decodeAudioPacket maxAudioFrameSize

-- | AVPacket struct
newtype AVPacket = AVPacket (ForeignPtr AVPacket)

instance ExternalPointer AVPacket where
	withThis (AVPacket pkt) io = withForeignPtr pkt (io . castPtr)

-- | Allocate an AVPacket.  Does not allocate any buffers
allocPacket :: MonadIO m => m AVPacket
allocPacket = liftIO$ do
	fp <- mallocForeignPtrBytes	#{size AVPacket}
	withForeignPtr fp$ \p -> av_init_packet p
	addForeignPtrFinalizer pav_free_packet fp
	return.AVPacket$ castForeignPtr fp

-- | Allocate packet with a buffer
newPacket :: (MonadIO m, MonadError String m) =>
	Int              -- ^ size of the buffer to allocate
	-> m AVPacket
newPacket size = do
	fp <- liftIO.mallocForeignPtrBytes$ #{size AVPacket}
	r <- liftIO.withForeignPtr fp$ \p -> av_new_packet p (fromIntegral size)

	if (r /= 0) then
		throwError$ "newPacket: error allocating new packet with size "
			++ (show size) ++ ", error code " ++ (show r)
	else liftIO$ do
		addForeignPtrFinalizer pav_free_packet fp
		return . AVPacket . castForeignPtr $ fp

-- TODO: is there a safe way to implement this function, so that it is
-- impossible to pass a freed packet to a libav function?
--cleanPacket :: AVPacket -> IO ()
--cleanPacket pkt = withThis pkt av_free_packet

-- |  get the stream_index field from an AVPacket
packetGetStreamIndex :: MonadIO m => AVPacket -> m Int
packetGetStreamIndex pkt = liftIO.withThis pkt $ \pkt' ->
	fromIntegral <$> (#{peek AVPacket, stream_index} pkt' :: IO CInt)

-- | get the size field from an AVPacket
packetGetSize :: MonadIO m => AVPacket -> m Int
packetGetSize pkt = liftIO.withThis pkt $ \pkt' ->
	fromIntegral <$> (#{peek AVPacket, size} pkt' :: IO CInt)

