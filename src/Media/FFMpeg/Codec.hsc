-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TupleSections #-}

{- |
 
Description : Bindings to libavcodec
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libavcodec.

-}

module Media.FFMpeg.Codec (
	libAVCodecVersion,

	module Media.FFMpeg.Codec.Enums,

	WithCodecId (..),
	WithCodecType (..),
	WithVideoProps (..),
	WithAudioProps (..),

	AVCodecContext,
	toCodecContext,

	AVCodec,
	openCodec,
	findDecoder,
	decodeVideo2,
	decodeAudio3,
	decodeAudioPacket,
	--maxAudioFrameSize,
	--decodeAudioPacket',

	CommonPicture (..),
	pictureGetSize,

	AVFrame,
	allocFrame,

	AVPacket,
	allocPacket,
	newPacket,
	cleanPacket,
	dupPacket,
	packetGetStreamIndex,
	packetGetSize,
	packetGetBuffer,
	packetSetBuffer
) where

#include "ffmpeg.h"

import Control.Applicative
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
import Media.FFMpeg.Common
import Media.FFMpeg.Util

-- | Which version of libavcodec are we using?
libAVCodecVersion :: Version
libAVCodecVersion = fromVersionNum #{const LIBAVCODEC_VERSION_INT}

-- TODO: review these classes, are they really safe?

-- | For structs that have a codecID field
class WithCodecId a where
	getCodecId :: a -> AVCodecId

-- | For structs that have a codecType field
class WithCodecType a where
	getCodecType :: a -> AVMediaType

-- | For structs that have video properties
class WithVideoProps a where
	getVideoWidth :: a -> Int
	getVideoHeight :: a -> Int
	getPixelFormat :: a -> PixelFormat

-- | For structs that have audio properties
class WithAudioProps a where
	getAudioSampleRate :: a -> Int
	getAudioChannels :: a -> Int
	getAudioSampleFormat :: a -> AVSampleFormat

-- | AVCodecContext struct
newtype AVCodecContext = AVCodecContext (ForeignPtr AVCodecContext)

instance ExternalPointer AVCodecContext where
	withThis (AVCodecContext ctx) io = withForeignPtr ctx (io . castPtr)

-- | convert pointer to AVCodecContext
toCodecContext :: Ptr a -> IO AVCodecContext
toCodecContext =  fmap AVCodecContext . newForeignPtr_ . castPtr

_unsafeGetValue :: ExternalPointer p => p -> (Ptr a -> IO b) -> b
_unsafeGetValue extp io = unsafePerformIO $ withThis extp io

instance WithCodecId AVCodecContext where
	getCodecId ctx = _unsafeGetValue ctx $ \ctx' ->
		toCEnum <$> (#{peek AVCodecContext, codec_id} ctx' :: IO CInt)

instance WithCodecType AVCodecContext where
	getCodecType ctx = _unsafeGetValue ctx $ \ctx' ->
		toCEnum <$> (#{peek AVCodecContext, codec_type} ctx' :: IO CInt)

instance WithVideoProps AVCodecContext where
	getVideoWidth ctx = _unsafeGetValue ctx $ \ctx' ->
		cToInt <$> (#{peek AVCodecContext, width} ctx' :: IO CInt)

	getVideoHeight ctx = _unsafeGetValue ctx $ \ctx' -> 
		cToInt <$> (#{peek AVCodecContext, height} ctx' :: IO CInt)

	getPixelFormat ctx = _unsafeGetValue ctx $ \ctx' -> 
		toCEnum <$> (#{peek AVCodecContext, pix_fmt} ctx' :: IO CInt)

instance WithAudioProps AVCodecContext where
	getAudioSampleRate ctx = _unsafeGetValue ctx $ \ctx' ->
		cToInt <$> (#{peek AVCodecContext, sample_rate} ctx' :: IO CInt)

	getAudioChannels ctx = _unsafeGetValue ctx $ \ctx' ->
		cToInt <$> (#{peek AVCodecContext, channels} ctx' :: IO CInt)

	getAudioSampleFormat ctx = _unsafeGetValue ctx $ \ctx' ->
		toCEnum <$> (#{peek AVCodecContext, sample_fmt} ctx' :: IO CInt)

-- | AVCodec struct
newtype AVCodec = AVCodec (Ptr AVCodec)

instance ExternalPointer AVCodec where
	withThis (AVCodec c) io = io (castPtr c)

instance WithCodecId AVCodec where
	getCodecId c = _unsafeGetValue c $ \c' ->
		toCEnum <$> (#{peek AVCodec, id} c' :: IO CInt)

instance WithCodecType AVCodec where
	getCodecType c = _unsafeGetValue c $ \c' ->
		toCEnum <$> (#{peek AVCodec, type} c' :: IO CInt)

-- | Initialize an AVCodecContext to use the given AVCodec
openCodec :: AVCodecContext -> AVCodec -> IO ()
openCodec ctx@(AVCodecContext ct) codec =
	withThis ctx $ \ctx' ->
	withThis codec $ \codec' -> do
		throwIf_ (<0)
			(printf "openCodec: fails to open codec '%s' - errocode %d\n" (show (getCodecId codec)) . cToInt)  
			(_avcodec_open ctx' codec')
		addForeignPtrFinalizer pavcodec_close (castForeignPtr ct)

-- TODO: upgrade to avcodec_open2
foreign import ccall "avcodec_open" _avcodec_open :: Ptr () -> Ptr () -> IO CInt
foreign import ccall "&avcodec_close" pavcodec_close :: FunPtr (Ptr () -> IO ())

-- | Find a registered decoder with a matching codec ID
findDecoder :: AVCodecId -> IO (Maybe AVCodec)
findDecoder cid = do
	(maybe Nothing (Just . AVCodec . castPtr) . justPtr) <$>
		(_avcodec_find_decoder (fromCEnum cid))

foreign import ccall "avcodec_find_decoder" _avcodec_find_decoder :: 
	CInt -> IO (Ptr ())

-- | Decode a video frame into a picture
-- Returns False if no frame could be decoded, otherwise True
decodeVideo2 :: AVCodecContext -> AVFrame -> AVPacket -> IO Bool
decodeVideo2 ctx frm pkt =
	withThis ctx $ \ctx' -> 
	withThis frm $ \frm' -> 
	withThis pkt $ \pkt' ->
	alloca $ \gotPic' -> do
		throwIf (< 0)
			(printf "decodeVideo: failed to decode video packet: %d" . cToInt)
			(_avcodec_decode_video ctx' frm' gotPic' pkt')
		fmap (/= 0) (peek gotPic')

foreign import ccall "avcodec_decode_video2" _avcodec_decode_video :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> IO CInt

-- | Decode an audio frame into samples
decodeAudio3 ::     
	AVCodecContext    -- ^ The codec context
	-> Buffer         -- ^ The output buffer
	-> AVPacket       -- ^ The input packet containing the input buffer
	-> IO (Int, Int)  -- ^ (number of bytes used, output buffer size)
decodeAudio3 ctx buf pkt = 
	withThis ctx $ \ctx' -> 
	withThis buf $ \buf' ->
	withThis pkt $ \pkt' -> 
	with bufSize $ \frm_size -> do
		result <- throwIf (< 0)
			(printf "decodeAudio: decode packet failed. Return value is %d" . cToInt)
			(cToInt <$> _avcodec_decode_audio ctx' buf' frm_size pkt')
		((result, ) . cToInt) <$> peek frm_size
	where bufSize = cFromInt $ bufferSize buf :: CInt

-- TODO: upgrade to avcodec_decode_audio4
foreign import ccall "avcodec_decode_audio3" _avcodec_decode_audio :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> IO CInt

--
-- | decodeAudio is very complex to use, the another version of the 
-- same function is decode audio packet. It decodes packets into a list of
-- buffers with max size is set to 'size'. 
--

decodeAudioPacket :: Int -> AVCodecContext -> AVPacket -> IO ByteString
decodeAudioPacket buffSize ctx pkt = dupPacket pkt >> fillBuffer 
    where 
      shiftPacket size = packetSetBuffer pkt =<< 
                         shiftBuffer size (packetGetBuffer pkt)
      copyToByteString buf size = 
          withBuffer buf $ \buf' ->
              packCStringLen (castPtr buf', size)

      fillBuffer = do
        buf <- allocBuffer buffSize
        (r, w) <- decodeAudio3 ctx buf pkt
        result <- copyToByteString buf w
        if r < (packetGetSize pkt) then do
                    shiftPacket r
                    rest <- fillBuffer
                    return $  result `append` rest
                  else return $ result

-- TODO: review these functions
      
--maxAudioFrameSize :: Int
--maxAudioFrameSize = #{const AVCODEC_MAX_AUDIO_FRAME_SIZE}

--
-- | decodeAudioPacket' - defaults the buffer size to 2048
--
--decodeAudioPacket' :: CodecContext -> Packet -> IO ByteString
--decodeAudioPacket' = decodeAudioPacket maxAudioFrameSize

-- TODO: review this class
class ExternalPointer pic => CommonPicture pic where
    pictureFill :: pic -> Buffer -> PixelFormat -> Int -> Int -> IO ()
    pictureFill pic buf pf w h =
        withThis pic $ \pic' -> 
        withThis buf $ \buf' ->
        _avpicture_fill (castPtr pic') 
             (castPtr buf')
             (fromCEnum pf)
             (cFromInt w) (cFromInt h) >> 
        return ()

    pictureGetSlice :: pic -> Ptr ()
    pictureGetSlice pic = 
        castPtr (_unsafeGetValue pic $ 
                 return . (`plusPtr` #{offset AVPicture, data}))
         
    pictureGetStride :: pic -> Ptr ()
    pictureGetStride pic = 
        castPtr (_unsafeGetValue pic $
                 return . (`plusPtr` #{offset AVPicture, linesize}))

foreign import ccall "avpicture_fill" _avpicture_fill ::
    Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO CInt

-- | Calculate the size of the picture in bytes
pictureGetSize :: PixelFormat -> Int -> Int -> Int
pictureGetSize pf w h = cToInt $ unsafePerformIO -- TODO: is this really safe?
	(_avpicture_get_size (fromCEnum pf) (cFromInt w) (cFromInt h))

foreign import ccall "avpicture_get_size" _avpicture_get_size :: 
    CInt -> CInt -> CInt -> IO CInt

-- | AVFrame struct
newtype AVFrame = AVFrame (ForeignPtr AVFrame)

instance ExternalPointer AVFrame where
    withThis (AVFrame f) io = withForeignPtr f (io . castPtr)

instance CommonPicture AVFrame

-- | Allocate an AVFrame
allocFrame :: IO AVFrame
allocFrame = do
	p <- throwIf (== nullPtr)
		(\_ -> "allocFrame: failed to allocate AVFrame")
		(_avcodec_alloc_frame)
	(AVFrame . castForeignPtr) <$> newAvForeignPtr p

foreign import ccall "avcodec_alloc_frame" _avcodec_alloc_frame :: IO (Ptr ())

-- | AVPacket struct
newtype AVPacket = AVPacket (ForeignPtr AVPacket)

instance ExternalPointer AVPacket where
	withThis (AVPacket pkt) io = withForeignPtr pkt (io . castPtr)

-- | Allocate packet structure. Does not allocate packet.data
allocPacket :: IO AVPacket
allocPacket = do
	p <- mallocBytes #{size AVPacket}
	_av_init_packet p
	pkt <- newAvForeignPtr p
	addForeignPtrFinalizer pav_free_packet pkt
	return (AVPacket $ castForeignPtr pkt)

-- The 0.51.0 version of ffmpeg defines av_init_packet as inline
-- TODO: review these functions
foreign import ccall "b_init_packet" _av_init_packet :: Ptr () -> IO ()
foreign import ccall "b_free_packet" _av_free_packet :: Ptr () -> IO ()
foreign import ccall "&b_free_packet" pav_free_packet ::
	FunPtr (Ptr () -> IO ())

-- | Allocate packet with data
newPacket :: Int -> IO AVPacket
newPacket s = do
	p <- mallocBytes #{size AVPacket}
	throwIf (/= 0)
		(printf "newPacket: error allocating new packet with size %d - %d" s . cToInt)
		(_av_new_packet p (cFromInt s))
	pkt <- newAvForeignPtr p
	addForeignPtrFinalizer pav_free_packet pkt
	return (AVPacket $ castForeignPtr pkt)

foreign import ccall "av_new_packet" _av_new_packet :: Ptr () -> CInt -> IO CInt

-- | Free the packet manually
cleanPacket :: AVPacket -> IO ()
cleanPacket pkt = withThis pkt _av_free_packet

-- | If packet doesn't own the buffer, then it creates new buffer, and copies
-- data there
dupPacket :: AVPacket -> IO ()
dupPacket pkt = 
	withThis pkt $ \pkt' -> do
		throwIf (< 0)
			(printf "dupPacket: failed to dup packet: %d" . cToInt)
			(_av_dup_packet pkt')
		return ()

foreign import ccall "av_dup_packet" _av_dup_packet :: Ptr ()-> IO CInt

-- |  get the stream_index field from an AVPacket
packetGetStreamIndex :: AVPacket -> Int
packetGetStreamIndex pkt = _unsafeGetValue pkt $ \pkt' ->
	cToInt <$> (#{peek AVPacket, stream_index} pkt' :: IO CInt)

-- | get the size field from an AVPacket
packetGetSize :: AVPacket -> Int
packetGetSize pkt = _unsafeGetValue pkt $ \pkt' ->
	cToInt <$> (#{peek AVPacket, size} pkt' :: IO CInt)

-- | get the data field from an AVPacket
packetGetBuffer :: AVPacket -> Buffer
packetGetBuffer pkt = 
	_unsafeGetValue pkt $ \pkt' -> 
		(#{peek AVPacket, data} pkt' :: IO (Ptr ())) >>= 
		castBufferSize (packetGetSize pkt)

-- | use a Buffer to set the size and data fields of an AVPacket
packetSetBuffer :: AVPacket -> Buffer -> IO ()
packetSetBuffer pkt buf = 
	withBuffer buf $ \buf' ->
	withThis pkt $ \pkt' -> do
		let bufSize = bufferSize buf
		#{poke AVPacket, size} pkt' ((cFromInt bufSize) :: CInt)
		#{poke AVPacket, data} pkt' (castPtr buf')

