-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |Module 'Media.FFMpeg.Codec' implements bindings to AVCodec library

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.Codec 
    (
     libAVCodecVersion

    ,module Media.FFMpeg.CodecEnums_

    ,WithCodecId (..)
    ,WithCodecType (..)
    ,WithVideoProps (..)
    ,WithAudioProps (..)

    ,CodecContext
    ,toCodecContext

    ,Codec
    ,openCodec
    ,findDecoder
    ,decodeVideo
    ,decodeAudio
    ,decodeAudioPacket
    --,maxAudioFrameSize
    ,decodeAudioPacket'

    ,CommonPicture (..)
    ,pictureGetSize

    ,Frame
    ,allocFrame

    ,Packet
    ,allocPacket
    ,newPacket
    ,cleanPacket
    ,dupPacket
    ,packetGetStreamIndex
    ,packetGetSize
    ,packetGetBuffer
    ,packetSetBuffer
    )where

#include "ffmpeg.h"

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (liftM)
import Text.Printf
import Data.Version
import Data.ByteString (ByteString, append, packCStringLen)

import Media.FFMpeg.Common
import Media.FFMpeg.Util
import Media.FFMpeg.CodecEnums_

libAVCodecVersion :: Version
libAVCodecVersion = fromVersionNum #{const LIBAVCODEC_VERSION_INT}

--
-- | properties classes
--
class WithCodecId a where
    getCodecId :: a -> CodecId

class WithCodecType a where
    getCodecType :: a -> CodecType

class WithVideoProps a where
    getVideoWidth :: a -> Int
    getVideoHeight :: a -> Int
    getPixelFormat :: a -> PixelFormat

class WithAudioProps a where
    getAudioSampleRate :: a -> Int
    getAudioChannels :: a -> Int
    getAudioSampleFormat :: a -> SampleFormat

-- 
-- | CodecContext -- wrapper to AVCodecContext
--
newtype CodecContext = CodecContext (ForeignPtr CodecContext)

instance ExternalPointer CodecContext where
    withThis (CodecContext ctx) io = withForeignPtr ctx (io . castPtr)

--
-- | convert poiner to CodecContext
toCodecContext :: Ptr a -> IO CodecContext
toCodecContext =  liftM CodecContext . newForeignPtr_ . castPtr

_unsafeGetValue :: ExternalPointer p => p -> (Ptr a -> IO b) -> b
_unsafeGetValue extp io = unsafePerformIO $ withThis extp io

instance WithCodecId CodecContext where
    getCodecId ctx = _unsafeGetValue ctx $ \ctx' ->
                     liftM cToEnum 
                     (#{peek AVCodecContext, codec_id} ctx' :: IO #{type enum AVCodecID})

instance WithCodecType CodecContext where
    getCodecType ctx = _unsafeGetValue ctx $ \ctx' ->
                       liftM cToEnum 
                       (#{peek AVCodecContext, codec_type} ctx' :: IO #{type enum AVMediaType})

instance WithVideoProps CodecContext where
    getVideoWidth ctx = _unsafeGetValue ctx $ \ctx' ->
                        liftM cToInt 
                                  (#{peek AVCodecContext, width} ctx' :: IO CInt)
                        

    getVideoHeight ctx = _unsafeGetValue ctx $ \ctx' -> 
                         liftM cToInt 
                                   (#{peek AVCodecContext, height} ctx' :: IO CInt)

    getPixelFormat ctx = _unsafeGetValue ctx $ \ctx' -> 
                         liftM cToEnum
                               (#{peek AVCodecContext, pix_fmt} ctx' :: IO #{type enum PixelFormat})

instance WithAudioProps CodecContext where
    getAudioSampleRate ctx = _unsafeGetValue ctx $ \ctx' ->
                             liftM cToInt (#{peek AVCodecContext, sample_rate} ctx' :: IO CInt)

    getAudioChannels ctx = _unsafeGetValue ctx $ \ctx' ->
                           liftM cToInt (#{peek AVCodecContext, channels} ctx' :: IO CInt)

    getAudioSampleFormat ctx = _unsafeGetValue ctx $ \ctx' ->
                               liftM cToEnum 
                                         (#{peek AVCodecContext, sample_fmt} ctx' :: IO #{type enum AVSampleFormat})

--
-- | Codec - implementation of AVCodec
--
newtype Codec = Codec (Ptr Codec)

instance ExternalPointer Codec where
    withThis (Codec c) io = io (castPtr c)

instance WithCodecId Codec where
    getCodecId c = _unsafeGetValue c $ \c' ->
                   liftM cToEnum 
                   (#{peek AVCodec, id} c' :: IO #{type enum AVCodecID})

instance WithCodecType Codec where
    getCodecType c = _unsafeGetValue c $ \c' ->
                     liftM cToEnum
                     (#{peek AVCodec, type} c' :: IO #{type enum AVMediaType})

--
-- | openCodec - opens codec due to CodecContext
--
foreign import ccall "avcodec_open" _avcodec_open :: Ptr () -> Ptr () -> IO CInt
foreign import ccall "avcodec_close" _avcodec_close :: Ptr () -> IO ()

openCodec :: CodecContext -> Codec -> IO ()
openCodec ctx @ (CodecContext ct) codec =
    withThis ctx $ \ctx' ->
        withThis codec $ \codec' -> do
          throwIf_ (<0)
                   (printf "openCodec: fails to open codec '%s' - errocode %d\n" (show (getCodecId codec)) . cToInt)  
                   (_avcodec_open ctx' codec')
          finalizer <- mkFinalizerPtr _avcodec_close
          addForeignPtrFinalizer finalizer (castForeignPtr ct)

--
-- |findDecoder - finds decoder by CodecId
--
foreign import ccall "avcodec_find_decoder" _avcodec_find_decoder :: 
    #{type enum AVCodecID} -> IO (Ptr ())

findDecoder :: CodecId -> IO (Maybe Codec)
findDecoder cid = do
  liftM (maybe Nothing (Just . Codec . castPtr) . justPtr)
        (_avcodec_find_decoder (cFromEnum cid))


--
-- |decodeVideo -- decodes video frame, returns True if
-- complete picture is decompressed
--
#if LIBAVCODEC_VERSION_MAJOR < 53
foreign import ccall "avcodec_decode_video" _avcodec_decode_video :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> CInt -> IO CInt

decodeVideo :: CodecContext -> Frame -> Packet -> IO Bool
decodeVideo ctx frm pkt =
    withThis ctx $ \ctx' -> 
    withThis frm $ \frm' -> 
    let obuf = packetGetBuffer pkt
    in withThis obuf $ \obuf' ->
    alloca $ \gotPic' -> do
      throwIf (<0)
              (printf "decodeVideo: failed to decode video packet: %d" . cToInt)
              (_avcodec_decode_video ctx' frm' gotPic' obuf' 
               (cFromInt (bufferSize obuf)))
      liftM (>0) (peek gotPic')
#else
foreign import ccall "avcodec_decode_video2" _avcodec_decode_video :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> IO CInt

decodeVideo :: CodecContext -> Frame -> Packet -> IO Bool
decodeVideo ctx frm pkt =
    withThis ctx $ \ctx' -> 
    withThis frm $ \frm' -> 
    withThis pkt $ \pkt' ->
    alloca $ \gotPic' -> do
      throwIf (<0)
              (printf "decodeVideo: failed to decode video packet: %d" . cToInt)
              (_avcodec_decode_video ctx' frm' gotPic' pkt')
      liftM (>0) (peek gotPic')
#endif

--
-- |decodeAudio - decodes audio frame to the existing buffer
-- return (ReadBytes, DecodedBytes)
--
#if LIBAVCODEC_VERSION_MAJOR < 53      
foreign import ccall "avcodec_decode_audio2" _avcodec_decode_audio :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> CInt -> IO CInt

decodeAudio :: CodecContext -> Buffer -> Packet -> IO (Int, Int)
decodeAudio ctx buf pkt = 
    withThis ctx $ \ctx' -> 
    withThis buf $ \buf' -> 
    let obuf = packetGetBuffer pkt 
    in withThis obuf $ \obuf' -> 
    with bufSize $ \frm_size -> do
      result <- throwIf (<0)
                (printf "decodeAudio: decode packet failed. Return value is %d" . cToInt)
                (liftM cToInt $ _avcodec_decode_audio ctx' buf' frm_size obuf' (cFromInt (bufferSize obuf)))
      liftM ((,) result . cToInt) $ peek frm_size
    where bufSize = cFromInt $ bufferSize buf :: CInt
#else
foreign import ccall "avcodec_decode_audio3" _avcodec_decode_audio :: 
    Ptr () -> Ptr () -> (Ptr CInt) -> Ptr () -> IO CInt

decodeAudio :: CodecContext -> Buffer -> Packet -> IO (Int, Int)
decodeAudio ctx buf pkt = 
    withThis ctx $ \ctx' -> 
    withThis buf $ \buf' ->
    withThis pkt $ \pkt' -> 
    with bufSize $ \frm_size -> do
      result <- throwIf (<0)
                (printf "decodeAudio: decode packet failed. Return value is %d" . cToInt)
                (liftM cToInt $ _avcodec_decode_audio ctx' buf' frm_size pkt')
      liftM ((,) result . cToInt) $ peek frm_size
    where bufSize = cFromInt $ bufferSize buf :: CInt
#endif

--
-- | decodeAudio is very complex to use, the another version of the 
-- same function is decode audio packet. It decodes packets into a list of
-- buffers with max size is set to 'size'. 
--

decodeAudioPacket :: Int -> CodecContext -> Packet -> IO ByteString
decodeAudioPacket buffSize ctx pkt = dupPacket pkt >> fillBuffer 
    where 
      shiftPacket size = packetSetBuffer pkt =<< 
                         shiftBuffer size (packetGetBuffer pkt)
      copyToByteString buf size = 
          withBuffer buf $ \buf' ->
              packCStringLen (castPtr buf', size)

      fillBuffer = do
        buf <- allocBuffer buffSize
        (r, w) <- decodeAudio ctx buf pkt
        result <- copyToByteString buf w
        if r < (packetGetSize pkt) then do
                    shiftPacket r
                    rest <- fillBuffer
                    return $  result `append` rest
                  else return $ result
      

--maxAudioFrameSize :: Int
--maxAudioFrameSize = #{const AVCODEC_MAX_AUDIO_FRAME_SIZE}

--
-- | decodeAudioPacket' - defaults the buffer size to 2048
--
--decodeAudioPacket' :: CodecContext -> Packet -> IO ByteString
--decodeAudioPacket' = decodeAudioPacket maxAudioFrameSize

--
-- |AVPicture
--

foreign import ccall "avpicture_fill" _avpicture_fill ::
    Ptr () -> Ptr () -> CInt -> CInt -> CInt -> IO CInt

class ExternalPointer pic => CommonPicture pic where
    pictureFill :: pic -> Buffer -> PixelFormat -> Int -> Int -> IO ()
    pictureFill pic buf pf w h =
        withThis pic $ \pic' -> 
        withThis buf $ \buf' ->
        _avpicture_fill (castPtr pic') 
             (castPtr buf')
             (cFromEnum pf)
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

--
-- |pictureGetSize -- calculate the size of the picture in bytes
--
foreign import ccall "avpicture_get_size" _avpicture_get_size :: 
    #{type enum PixelFormat} -> CInt -> CInt -> IO CInt

pictureGetSize :: PixelFormat -> Int -> Int -> Int
pictureGetSize pf w h = cToInt $ unsafePerformIO 
                        (_avpicture_get_size (cFromEnum pf) (cFromInt w) (cFromInt h))

--
-- |Frame - implementation of AVFrame
--

newtype Frame = Frame (ForeignPtr Frame)

instance ExternalPointer Frame where
    withThis (Frame f) io = withForeignPtr f (io . castPtr)

instance CommonPicture Frame

--
-- |allocFrame - allocate frame structure
--
foreign import ccall "avcodec_alloc_frame" _avcodec_alloc_frame :: IO (Ptr ())

allocFrame :: IO Frame
allocFrame = do
  p <- throwIf (== nullPtr)
               (\_ -> "allocFrame: failed to allocate frame")
               _avcodec_alloc_frame
  liftM (Frame . castForeignPtr) $ newAvForeignPtr p

--
-- | Packet -- binding to AVPacket
--

newtype Packet = Packet (ForeignPtr Packet)

instance ExternalPointer Packet where
    withThis (Packet pkt) io = withForeignPtr pkt (io . castPtr)

--
-- |allocPacket -- allocates packet structure, but don't allocates the
-- packet.data
--

-- The 0.51.0 version of ffmpeg defines av_init_packet as inline
foreign import ccall "b_init_packet" _av_init_packet :: Ptr () -> IO ()
foreign import ccall "b_free_packet" _b_free_packet :: Ptr () -> IO ()


allocPacket :: IO Packet
allocPacket = do
  p <- mallocBytes #{size AVPacket}
  _av_init_packet p
  pkt <- newAvForeignPtr p
  deinit <- mkFinalizerPtr _b_free_packet 
  addForeignPtrFinalizer deinit pkt
  return (Packet $ castForeignPtr pkt)

--
-- |newPacket -- allocates packet with data
-- 
foreign import ccall "av_new_packet" _av_new_packet :: Ptr () -> CInt -> IO CInt

newPacket :: Int -> IO Packet
newPacket s = do
  p <- mallocBytes #{size AVPacket}
  throwIf (/= 0)
          (printf "newPacket: error allocating new packet with size %d - %d" s . cToInt)
          (_av_new_packet p (cFromInt s))
  pkt <- newAvForeignPtr p
  deinit <- mkFinalizerPtr _b_free_packet
  addForeignPtrFinalizer deinit pkt
  return (Packet $ castForeignPtr pkt)

--
-- | cleanPacket - cleans the packet manually
--
cleanPacket :: Packet -> IO ()
cleanPacket pkt = withThis pkt _b_free_packet


--
-- | dupPacket -- if packet doesn't own the buffer, than it 
-- creates new buffer, and copies data there
--
foreign import ccall "av_dup_packet" _av_dup_packet :: Ptr ()-> IO CInt

dupPacket :: Packet -> IO ()
dupPacket pkt = 
    withThis pkt $ \pkt' -> 
        throwIf (< 0)
                (printf "dupPacket: failed to dup packet: %d" . cToInt)
                (_av_dup_packet pkt') >> return ()

packetGetStreamIndex :: Packet -> Int
packetGetStreamIndex pkt = _unsafeGetValue pkt $ \pkt' ->
                           liftM cToInt (#{peek AVPacket, stream_index} pkt' :: IO CInt)

packetGetSize :: Packet -> Int
packetGetSize pkt = _unsafeGetValue pkt $ \pkt' ->
                    liftM cToInt (#{peek AVPacket, size} pkt' :: IO CInt)

packetGetBuffer :: Packet -> Buffer
packetGetBuffer pkt = 
    _unsafeGetValue pkt $ \pkt' -> 
        (#{peek AVPacket, data} pkt' :: IO (Ptr ())) >>= 
        castBufferSize (packetGetSize pkt)

packetSetBuffer :: Packet -> Buffer -> IO ()
packetSetBuffer pkt buf = 
    withBuffer buf $ \buf' ->
    withThis pkt $ \pkt' -> do
                            let bufSize = bufferSize buf
                            #{poke AVPacket, size} pkt' ((cFromInt bufSize) :: CInt)
                            #{poke AVPacket, data} pkt' (castPtr buf')
