-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts #-}

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
	openInputFile,

	AVStream,
	getStreams,
	readFrame
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
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

import Media.FFMpeg.Codec
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util
import Media.FFMpeg.Util.Dict

-- | Initialize libavformat and register all the muxers, demuxers and protocols
foreign import ccall "av_register_all" registerAll :: IO ()
foreign import ccall "av_find_stream_info" find_stream_info :: Ptr () -> IO CInt
foreign import ccall "dump_format" dump_format :: 
	Ptr () -> CInt -> CString -> CInt -> IO ()
foreign import ccall "avformat_open_input" avformat_open_input:: 
	Ptr (Ptr ()) -> CString -> Ptr () -> Ptr (Ptr ()) -> IO CInt
foreign import ccall "&avformat_close_input" pavformat_close_input::
	FunPtr (Ptr a -> IO ())
foreign import ccall "av_read_frame" av_read_frame :: Ptr () -> Ptr () -> IO CInt

-- | Which version of libavformat are we using?
libAVFormatVersion :: Version
libAVFormatVersion = fromVersionNum #{const LIBAVFORMAT_VERSION_INT}

-- | AVFormatContext struct
newtype AVFormatContext = AVFormatContext (ForeignPtr AVFormatContext)

instance ExternalPointer AVFormatContext where
	withThis (AVFormatContext fmt) io = withForeignPtr fmt (io . castPtr)

-- | findStreamInfo
-- TODO: upgrade
findStreamInfo :: (MonadIO m, MonadError String m) => AVFormatContext -> m ()
findStreamInfo fmt = do
	r <- liftIO.withThis fmt $ \fmt' -> find_stream_info fmt'

	when (r < 0) $ do
		throwError$ "findStreamInfo: failed with error code " ++ (show r)

-- | Output format information to the console (for input streams)
dumpInputFormat :: MonadIO m => AVFormatContext -> FilePath -> m ()
dumpInputFormat = dumpFormat 0

-- | Output format information to the console (for output streams)
dumpOutputFormat :: MonadIO m => AVFormatContext -> FilePath -> m ()
dumpOutputFormat = dumpFormat 1

-- | Dump format information for all streams
dumpFormat :: MonadIO m => CInt -> AVFormatContext -> FilePath -> m ()
dumpFormat io fmt s = liftIO$
	withThis fmt $ \fmt' ->
	withCString s $ \s' -> do
		count <- fromIntegral <$>
			(#{peek AVFormatContext, nb_streams} fmt' :: IO CUInt)
		forM_ [1..count] $ \i -> dump_format fmt' i s' io

-- | Open a media file
openInputFile :: (MonadIO m, MonadError String m) =>
	FilePath                   -- ^file to open
	-> AVDictionary            -- ^dictionary containing options for the demuxer
	-> m AVFormatContext       -- ^a new AVFormatContext structure
openInputFile name dict = do
	pp <- liftIO malloc
	liftIO$ poke pp nullPtr
	
	r <- liftIO$
		withCString name $ \s ->
		withThis dict $ \pd -> avformat_open_input pp s nullPtr pd

	if r /= 0 then do
		liftIO$ free pp
		throwError$ "openInputFile: failed to open \"" ++ name
			++ "\" with errorcode " ++ (show r)
	else liftIO$ do
		fp <- newForeignPtr pavformat_close_input =<< peek pp
		free pp
		return . AVFormatContext . castForeignPtr $ fp

-- | AVStream struct
newtype AVStream = AVStream (Ptr AVStream)

instance ExternalPointer AVStream where
	withThis (AVStream s) io = io (castPtr s)

-- | Retrieve all streams from the AVFormatContext
getStreams :: MonadIO m => AVFormatContext -> m [AVStream]
getStreams fmt = liftIO$
	withThis fmt $ \fmt' -> do
		count <- fromIntegral <$>
			(#{peek AVFormatContext, nb_streams} fmt' :: IO CUInt)
		let streamsPtr = fmt' `plusPtr` #{offset AVFormatContext, streams}
		(fmap AVStream) <$> peekArray count streamsPtr

-- | Read a frame from the file.
-- Returns False if there is an error or EOF occured, otherwise True
readFrame :: (MonadIO m) => AVFormatContext -> AVPacket -> m Bool
readFrame fmt pkt = liftIO$
	withThis fmt $ \fmt' ->
	withThis pkt $ \pkt' -> do
		r <- av_read_frame fmt' pkt'
		return$ r >= 0

