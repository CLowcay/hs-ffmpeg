-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |

Description : Bindings to libavformat
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format (
	libAVFormatVersion,
	registerAll,

	AVFormatContext,
	findStreamInfo,
	dumpFormat,
	newContext,
	openInputFile,

	AVStream,
	getStreams,
	getCodecContext,
	readFrame
) where

#include "ffmpeg.h"

import Control.Applicative
import Data.Version
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Text.Printf (printf)

import Media.FFMpeg.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Codec

-- | Which version of libavformat are we using?
libAVFormatVersion :: Version
libAVFormatVersion = fromVersionNum #{const LIBAVFORMAT_VERSION_INT}

-- | Initialize libavformat and register all the muxers, demuxers and protocols
foreign import ccall "av_register_all" registerAll :: IO ()

-- | AVFormatContext struct
newtype AVFormatContext = AVFormatContext (ForeignPtr AVFormatContext)

instance ExternalPointer AVFormatContext where
	withThis (AVFormatContext fmt) io = withForeignPtr fmt (io . castPtr)

-- | findStreamInfo
findStreamInfo :: AVFormatContext -> IO ()
findStreamInfo fmt = 
	withThis fmt $ \fmt' ->
		throwIf_ (< 0) 
			(printf "findStreamInfo: failed with error code %d\n" . cToInt) 
			(_find_stream_info fmt')

foreign import ccall "av_find_stream_info" _find_stream_info :: Ptr () -> IO CInt

-- | Output format information to the console
dumpFormat :: AVFormatContext -> String -> IO ()
dumpFormat fmt s =
	withThis fmt $ \fmt' ->
	withCString s $ \s' ->
		_dump_format fmt' 0 s' 0

foreign import ccall "dump_format" _dump_format :: 
    Ptr () -> CInt -> CString -> CInt -> IO ()


-- | Allocate a new AVFormatContext
newContext :: IO AVFormatContext
newContext = do 
	p <- throwIf (== nullPtr) 
		(\_ -> "newContext: failed to allocate context") 
		_alloc_context
	(AVFormatContext . castForeignPtr) <$> newAvForeignPtr p

#if LIBAVFORMAT_VERSION_MAJOR < 53
foreign import ccall "av_alloc_format_context" _alloc_context :: IO (Ptr ())
#else
foreign import ccall "avformat_alloc_context" _alloc_context :: IO (Ptr ())
#endif

-- | Open a media file
openInputFile :: String -> IO AVFormatContext
openInputFile name = 
	withCString name $ \s ->
		alloca $ \pp -> do
			throwIf_ (/= 0) 
				(printf "openInputFile: fail to open %s - errorcode %d\n" name . cToInt)
				(_open_file pp s nullPtr 0 nullPtr)
			ptr <- peek pp
			(AVFormatContext . castForeignPtr) <$> newFinForeignPtr _close_file ptr

foreign import ccall "av_open_input_file" _open_file :: 
	Ptr (Ptr ()) -> CString -> Ptr () -> CInt -> Ptr () -> IO CInt

foreign import ccall "av_close_input_file" _close_file ::
	Ptr a -> IO ()

-- | AVStream struct
newtype AVStream = AVStream (Ptr AVStream)

instance ExternalPointer AVStream where
	withThis (AVStream s) io = io (castPtr s)

-- | Retrieve all streams from the AVFormatContext
getStreams :: AVFormatContext -> IO [AVStream]
getStreams fmt =
	withThis fmt $ \fmt' -> do
		count <- fromIntegral <$>
			(#{peek AVFormatContext, nb_streams} fmt' :: IO CUInt)
		let streamsPtr = fmt' `plusPtr` #{offset AVFormatContext, streams}
		(fmap AVStream) <$> peekArray count streamsPtr

-- | Read the AVCodecContext from an AVStream structure
getCodecContext :: AVStream -> Maybe AVCodecContext
getCodecContext s = unsafePerformIO $ -- safe because AVStream is immutable (mostly)
	withThis s $ \s' -> do
		p <- justPtr <$> (#{peek AVStream, codec} s' :: IO (Ptr ()))
		maybe (return Nothing) (fmap Just . toCodecContext) p
                           
-- | Read a frame from the file.
-- Returns False if there is an error or EOF occured, otherwise True
readFrame :: AVFormatContext -> AVPacket -> IO Bool
readFrame fmt pkt = 
	withThis fmt $ \fmt' ->
	withThis pkt $ \pkt' ->
		(>= 0) <$> _read_frame fmt' pkt'

foreign import ccall "av_read_frame" _read_frame :: Ptr () -> Ptr () -> IO CInt

