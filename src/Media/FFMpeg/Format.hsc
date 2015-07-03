-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |Module 'Media.FFMpeg.Format' implements bindings to AVFormat library

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.Format 
    (
     libAVFormatVersion
    ,registerAll
    ,withFFMpeg

    ,FormatContext
    ,findStreamInfo
    ,dumpFormat
    ,newContext
    ,openInputFile

    ,Stream
    ,getStreams
    ,getCodecContext
    ,readFrame
    ) where

#include "ffmpeg.h"

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Version
import Control.Monad (liftM)
import Text.Printf (printf)

import Media.FFMpeg.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Codec

libAVFormatVersion :: Version
libAVFormatVersion = fromVersionNum #{const LIBAVFORMAT_VERSION_INT}

--
-- | registerAll, this should be the first instruction in before working wiht
-- ffmpeg library
--
foreign import ccall "av_register_all" registerAll :: IO ()

--
-- | control helper
--
withFFMpeg :: IO a -> IO a
withFFMpeg io = registerAll >> io


-- |FormatContext - wrapper to AVFormatContext
newtype FormatContext = FormatContext (ForeignPtr FormatContext)

instance ExternalPointer FormatContext where
    withThis (FormatContext fmt) io = withForeignPtr fmt (io . castPtr)

--
-- | findStreamInfo
foreign import ccall "av_find_stream_info" _find_stream_info :: Ptr () -> IO CInt

findStreamInfo :: FormatContext -> IO ()
findStreamInfo fmt = 
    withThis fmt $ \fmt' -> 
        throwIf_ (< 0) 
                     (printf "findStreamInfo: failed with error code %d\n" . cToInt) 
                     (_find_stream_info fmt')

-- 
-- | dumpFormat - outputs the format into console
--
foreign import ccall "dump_format" _dump_format :: 
    Ptr () -> CInt -> CString -> CInt -> IO ()

dumpFormat :: FormatContext -> String -> IO ()
dumpFormat fmt s =
    withThis fmt $ \fmt' ->
        withCString s $ \s' ->
            _dump_format fmt' 0 s' 0


--
-- | newContext - allocates new FormatContext
--
#if LIBAVFORMAT_VERSION_MAJOR < 53
foreign import ccall "av_alloc_format_context" _alloc_context :: IO (Ptr ())
#else
foreign import ccall "avformat_alloc_context" _alloc_context :: IO (Ptr ())
#endif

newContext :: IO FormatContext
newContext = do 
  p <- throwIf (== nullPtr) 
               (\_ -> "allocContext: failed to allocate context") 
               _alloc_context
  liftM (FormatContext . castForeignPtr) $ newAvForeignPtr p

--
-- |openInputFile -- opens media file
--
foreign import ccall "av_open_input_file" _open_file :: 
    Ptr (Ptr ()) -> CString -> Ptr () -> CInt -> Ptr () -> IO CInt

foreign import ccall "av_close_input_file" _close_file ::
    Ptr a -> IO ()

openInputFile :: String -> IO FormatContext
openInputFile name = 
    withCString name $ \s ->
        alloca $ \pp -> do
          throwIf_ (/=0) 
                       (printf "openInputFile: fail to open %s - errorcode %d\n" name . cToInt) $
                       _open_file pp s nullPtr 0 nullPtr
          ptr <- peek pp
          liftM (FormatContext . castForeignPtr) $ newFinForeignPtr _close_file ptr


--
-- |Stream - wrapper to AVStream
--
newtype Stream = Stream (Ptr Stream)

instance ExternalPointer Stream where
    withThis (Stream s) io = io (castPtr s)

--
-- |getStreams -- retrieve all streams from the FormatContext
--
getStreams :: FormatContext -> [Stream]
getStreams fmt = unsafePerformIO $
                 withThis fmt $ \fmt' -> do
                   count <- liftM fromIntegral $ 
                            (#{peek AVFormatContext, nb_streams} fmt' :: IO CUInt)
                   let streamsPtr = fmt' `plusPtr` #{offset AVFormatContext, streams}
                   liftM (map Stream) $ peekArray count streamsPtr

--
-- |getCodecContext - reads codec context from the stream structure
--
getCodecContext :: Stream -> Maybe CodecContext
getCodecContext s = unsafePerformIO $ 
                    withThis s $ \s' -> do
                      p <- liftM justPtr $ (#{peek AVStream, codec} s' :: IO (Ptr ()))
                      maybe (return Nothing) (liftM Just . toCodecContext) p
                           

--
-- |readFrame - reads frame from file, return False if error of EOF occured,
-- True otherwise
--
foreign import ccall "av_read_frame" _read_frame :: Ptr () -> Ptr () -> IO CInt

readFrame :: FormatContext -> Packet -> IO Bool
readFrame fmt pkt = 
    withThis fmt $ \fmt' ->
        withThis pkt $ \pkt' ->
            liftM (>= 0) $ _read_frame fmt' pkt'