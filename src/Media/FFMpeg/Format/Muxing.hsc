{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Muxing (
	writeHeader,
	writeFrame,
	interleavedWriteFrame,
	writeUncodedFrame,
	flushFrames,
	interleavedFlushFrames,
	flushUncodedFrames,
	interleavedFlushUncodedFrames,
	canWriteUncodedFrames,
	writeTrailer,
	guessFormat,
	guessCodec,
	getOutputTimestamp
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "avformat_write_header" avformat_write_header :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt
foreign import ccall "av_write_frame" av_write_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt
foreign import ccall "av_interleaved_write_frame" av_interleaved_write_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt
foreign import ccall "av_write_uncoded_frame" av_write_uncoded_frame :: Ptr AVFormatContext -> CInt -> Ptr AVFrame -> IO CInt
foreign import ccall "av_interleaved_write_uncoded_frame" av_interleaved_write_uncoded_frame :: Ptr AVFormatContext -> CInt -> Ptr AVFrame -> IO CInt
foreign import ccall "av_write_uncoded_frame_query" av_write_uncoded_frame_query :: Ptr AVFormatContext -> CInt -> IO CInt
foreign import ccall "av_write_trailer" av_write_trailer :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_guess_format" av_guess_format :: CString -> CString -> CString -> IO (Ptr AVOutputFormat)
foreign import ccall "av_guess_codec" av_guess_codec :: Ptr AVOutputFormat -> CString -> CString -> CString -> CInt -> IO CInt
foreign import ccall "av_get_output_timestamp" av_get_output_timestamp :: Ptr AVFormatContext -> CInt -> Ptr Int64 -> Ptr Int64 -> IO CInt

-- | Write the header to the output file.  The AVDictionary contains options
-- for the muxer.  On return it will contain only the options that were not
-- found.
writeHeader :: (MonadIO m, MonadError String m) => AVFormatContext -> Maybe AVDictionary -> m ()
writeHeader ctx dict =
	withThis ctx$ \pctx ->
	withOrNull dict$ \ppdict -> do
		r <- liftIO$ avformat_write_header pctx ppdict
		when (r /= 0)$ throwError$
			"writeHeader: avformat_write_header failed with error code " ++ (show r)

-- | Write a packet to the output file.
writeFrame :: (MonadIO m, MonadError String m) => AVFormatContext -> AVPacket -> m ()
writeFrame ctx pkt = do
	r <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt -> av_write_frame pctx ppkt
	
	when (r < 0)$ throwError$
		"writeFrame: av_write_frame failed with error code " ++ (show r)

-- | Write a frame using correct interleaving.
interleavedWriteFrame :: (MonadIO m, MonadError String m) =>
	AVFormatContext -> AVPacket -> m ()
interleavedWriteFrame ctx pkt = do
	r <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt -> do
			r <- av_interleaved_write_frame pctx ppkt
			#{poke AVPacket, buf} ppkt nullPtr
			#{poke AVPacket, data} ppkt nullPtr
			#{poke AVPacket, size} ppkt (0 :: CInt)
			return r

	when (r < 0)$ throwError$
		"interleavedWriteFrame: av_interleaved_write_frame failed with error code " ++ (show r)

-- | Write a raw frame directly to output
writeUncodedFrame :: (MonadIO m, MonadError String m) =>
	AVFormatContext -> AVFrame -> StreamIndex -> m ()
writeUncodedFrame ctx pkt (StreamIndex idx) = do
	r <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt -> av_write_uncoded_frame pctx idx ppkt
	
	when (r < 0)$ throwError$
		"writeUncodedFrame: av_write_uncoded_frame failed with error code " ++ (show r)

-- | Write a raw frame directly to output with correct interleaving
--interleavedWriteUncodedFrame :: (MonadIO m, MonadError String m) =>
--	AVFormatContext -> AVFrame -> StreamIndex -> m ()
--interleavedWriteUncodedFrame ctx frame (StreamIndex idx) = do
--	r <- liftIO$
--		withThis ctx$ \pctx ->
--		withThis pkt$ \ppkt -> av_interleaved_write_uncoded_frame pctx idx ppkt
--
--	when (r < 0)$ throwError$
--		"interleavedWriteUncodedFrame: av_interleaved_write_uncoded_frame failed with error code " ++ (show r)

-- | Flush all the frames.  Returns True if all data has been flushed.
flushFrames :: (MonadIO m, MonadError String m) => AVFormatContext -> m Bool
flushFrames ctx = do
	r <- liftIO.withThis ctx$ \pctx -> av_write_frame pctx nullPtr
	if r == 0 then return False
	else if r == 1 then return True
	else throwError$
		"flushFrames: av_write_frame failed with error code " ++ (show r)

-- | Flush all buffered frames for interleaving.  Returns True if all the data
-- has been flushed
interleavedFlushFrames :: (MonadIO m, MonadError String m) => AVFormatContext -> m Bool
interleavedFlushFrames ctx = do
	r <- liftIO.withThis ctx$ \pctx -> av_interleaved_write_frame pctx nullPtr
	if r == 0 then return False
	else if r == 1 then return True
	else throwError$
		"interleavedFlushFrames: av_interleaved_write_frame failed with error code " ++ (show r)

-- | Flush uncoded frames.  Returns True if all data has been flushed.
flushUncodedFrames :: (MonadIO m, MonadError String m) =>
	AVFormatContext -> StreamIndex -> m Bool
flushUncodedFrames ctx (StreamIndex idx) = do
	r <- liftIO.withThis ctx$ \pctx -> av_write_uncoded_frame pctx idx nullPtr
	if r == 0 then return False
	else if r == 1 then return True
	else throwError$
		"flushUncodedFrames: av_write_uncoded_frame failed with error code " ++ (show r)

-- | Flush all uncoded frames buffered for interleaving.  Returns True if all
-- the data has been flushed.
interleavedFlushUncodedFrames :: (MonadIO m, MonadError String m) =>
	AVFormatContext -> StreamIndex -> m Bool
interleavedFlushUncodedFrames ctx (StreamIndex idx) = do
	r <- liftIO.withThis ctx$ \pctx -> av_interleaved_write_uncoded_frame pctx idx nullPtr
	if r == 0 then return False
	else if r == 1 then return True
	else throwError$
		"interleavedFlushUncodedFrames: av_interleaved_write_uncoded_frame failed with error code " ++ (show r)

-- | Determine if the output format and stream can handle uncoded frames.
canWriteUncodedFrames :: MonadIO m => AVFormatContext -> StreamIndex -> m Bool
canWriteUncodedFrames ctx (StreamIndex idx) = do
	r <- liftIO.withThis ctx$ \pctx -> av_write_uncoded_frame_query pctx idx
	return$ r >= 0

-- | Write stream trailer.  Must be called after a successful writeHeader.
writeTrailer :: (MonadIO m, MonadError String m) => AVFormatContext -> m ()
writeTrailer ctx = do
	r <- liftIO.withThis ctx$ \pctx -> av_write_trailer pctx
	when (r /= 0)$ throwError$ "writeTrailer: av_write_trailer failed with error code " ++ (show r)

-- | Attempt to guess an output format.
guessFormat :: MonadIO m =>
	Maybe String                     -- ^ Short name to attempt to match
	-> Maybe String                  -- ^ File name to attempt to match 
	-> Maybe String                  -- ^ mime-type to attempt to match
	-> m (Maybe AVOutputFormat)
guessFormat mshortName mfilename mmimeType = do
	r <- liftIO$
		withCStringOrNull mshortName$ \pshortName ->
		withCStringOrNull mfilename$ \pfilename  ->
		withCStringOrNull mmimeType$ \pmimeType  ->
			av_guess_format pshortName pfilename pmimeType
	if r == nullPtr then return Nothing else return.Just$ AVOutputFormat r

withCStringOrNull :: Maybe String -> (CString -> IO b) -> IO b
withCStringOrNull (Just s) io = withCString s io
withCStringOrNull Nothing io = io nullPtr

-- | Guess the codec to use for a given output format.
guessCodec :: MonadIO m =>
	AVOutputFormat         -- ^ Output format
	-> Maybe String        -- ^ Short name to attempt match
	-> Maybe String        -- ^ File name to attempt to match
	-> Maybe String        -- ^ mime-type to attempt to match
	-> AVMediaType         -- ^ media type to match
	-> m (Maybe AVCodecID)
guessCodec (AVOutputFormat ouf) mshortName mfilename mmimeType mt = do
	r <- liftIO$
		withCStringOrNull mshortName$ \pshortName ->
		withCStringOrNull mfilename$ \pfilename ->
		withCStringOrNull mmimeType$ \pmimeType -> toCEnum <$>
			av_guess_codec ouf pshortName pfilename pmimeType (fromCEnum mt)
	if r == AVCodecIdNone then return Nothing else return$ Just r

-- | Simultaneously measure the dts and wall time of the last packet that was
-- output.
getOutputTimestamp :: (MonadIO m, MonadError String m) =>
	AVFormatContext -> StreamIndex -> m (AVTimestamp, AVTimestamp)
getOutputTimestamp ctx (StreamIndex idx) = do
	(r, dts, wall) <- liftIO$
		withThis ctx$ \pctx ->
		alloca$ \pdts ->
		alloca$ \pwall -> do
			r <- av_get_output_timestamp pctx idx pdts pwall
			dts <- peek pdts
			wall <- peek pwall
			return (r, dts, wall)
	when (r /= 0)$ throwError$
		"getOutputTimestamp: av_get_output_timestamp failed with error code " ++ (show r)
	return (AVTimestamp dts, AVTimestamp wall)

