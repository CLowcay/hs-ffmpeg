{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.Demuxing (
	findInputFormat,
	openInput,
	findStreamInfo,
	findPrograms,
	findBestStream,
	readFrame,
	seekKeyframe,
	seekFile,
	--formatFlush,
	readPlay,
	readPause
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont hiding (MonadIO, liftIO)
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Int
import Data.Monoid
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Map as M

import Media.FFMpeg.Codec
import Media.FFMpeg.Format.Core
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

foreign import ccall "av_find_input_format" av_find_input_format :: CString -> IO (Ptr AVInputFormat)
foreign import ccall "avformat_open_input" avformat_open_input :: Ptr (Ptr AVFormatContext) -> CString -> Ptr AVInputFormat -> Ptr (Ptr AVDictionary) -> IO CInt
foreign import ccall "avformat_find_stream_info" avformat_find_stream_info :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt
foreign import ccall "av_find_program_from_stream" av_find_program_from_stream :: Ptr AVFormatContext -> Ptr AVProgram -> CInt -> IO (Ptr AVProgram)
foreign import ccall "av_find_best_stream" av_find_best_stream :: Ptr AVFormatContext -> CInt -> CInt -> CInt -> Ptr (Ptr AVCodec) -> CInt -> IO CInt
foreign import ccall "av_read_frame" av_read_frame :: Ptr AVFormatContext -> Ptr AVPacket -> IO CInt
foreign import ccall "av_seek_frame" av_seek_frame :: Ptr AVFormatContext -> CInt -> Int64 -> CInt -> IO CInt
foreign import ccall "avformat_seek_file" avformat_seek_file :: Ptr AVFormatContext -> CInt -> Int64 -> Int64 -> Int64 -> CInt -> IO CInt
--foreign import ccall "avformat_flush" avformat_flush :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_read_play" av_read_play :: Ptr AVFormatContext -> IO CInt
foreign import ccall "av_read_pause" av_read_pause :: Ptr AVFormatContext -> IO CInt
foreign import ccall "&b_avformat_close_input" pavformat_close_input :: FunPtr (Ptr () -> IO ())

-- | Get the input format with the given name
findInputFormat :: MonadIO m => String -> m (Maybe AVInputFormat)
findInputFormat name = do
	r <- liftIO$ withCString name av_find_input_format
	if r == nullPtr then return Nothing else return.(Just).AVInputFormat$ r

-- | Open an input stream.  Returns the AVFormatContext and a dictionary as
-- described in the ffmpeg docs
openInput :: (MonadIO m, MonadThrow m) =>
	FilePath
	-> Maybe AVInputFormat
	-> Maybe AVDictionary
	-> m (AVFormatContext, AVDictionary)
openInput path mif mdict = do
	(r, ctx, dict) <- 
		withOrNull mif$ \pif ->
		withOrNull mdict$ \ppdict0 ->
		withThis path$ \ppath -> liftIO$
			alloca$ \ppdict ->
			alloca$ \pctx -> do
				pdict0 <- if ppdict0 == nullPtr then return nullPtr else peek ppdict0
				av_dict_copy ppdict pdict0 0
				r <- avformat_open_input pctx ppath pif ppdict
				ctx <- peek pctx
				dict <- peek ppdict
				return (r, ctx, dict)

	when (r /= 0)$ throwM$ mkError r "openInput" "avformat_open_input"

	rdict <- unsafeDictCopyFromPtr dict mempty 
	liftIO$ with dict av_dict_free
	rctx <- mkAVFormatContextFromPtr ctx (Just pavformat_close_input)
	return (rctx, rdict)

-- | Get information about all the streams in a format context.  Attempts to
-- open codecs for all the streams.  Optionally takes an AVDictionary of
-- options for each codec, and returns AVDictionaries containing the options
-- that were not recognised.
findStreamInfo :: (MonadIO m, MonadThrow m) =>
	AVFormatContext -> (M.Map StreamIndex AVDictionary) -> m [AVDictionary]
findStreamInfo ctx dicts = do
	defDict <- newAVDictionary
	let streams = if M.null dicts then []
		else [StreamIndex 0 .. last$ M.keys dicts]

	-- marshal in the dictionaries
	pa <- liftIO$
		nest (withThis <$> mkList dicts defDict streams)$ \pdicts -> do
			pa <- if null pdicts then return nullPtr else mallocArray$ length streams
			forM (pdicts `zip` [0..])$ \(ppdict, i) -> do
				pdict <- peek ppdict
				av_dict_copy (pa `advancePtr` i) pdict 0
			return pa

	r <- liftIO.withThis ctx$ \pctx -> avformat_find_stream_info pctx pa
	when (r < 0)$ throwM$ mkError r "findStreamInfo" "avformat_find_stream_info"
	
	-- marshal out the dictionaries
	mapM (\pdict -> unsafeDictCopyFromPtr pdict mempty)
		=<< (liftIO$ peekArray (length streams) pa)
	where
		mkList :: Ord a => M.Map a b -> b -> [a] -> [b]
		mkList ab def vs = (maybe def id).(flip M.lookup ab) <$> vs

		nest :: Monad m => [(r -> m a) -> m a] -> ([r] -> m a) -> m a
		nest xs = runContT (sequence (map ContT$ xs))

-- | Find all the programs associated with a stream
findPrograms :: MonadIO m => AVFormatContext -> StreamIndex -> m [ProgramID]
findPrograms ctx (StreamIndex idx) = do
	ptrs <- liftIO.withThis ctx$ \pctx ->
		let allPrograms prev = do
			next <- av_find_program_from_stream pctx prev idx
			if next == nullPtr then return [] else (next :) <$> allPrograms next
		in allPrograms nullPtr
	mapM (liftIO.(ProgramID <$>).(#{peek AVProgram, id})) ptrs

-- | Find the best stream for the given constraints.
findBestStream :: MonadIO m =>
	AVFormatContext          -- Format context
	-> AVMediaType           -- Media type
	-> Maybe StreamIndex     -- User requested stream
	-> Maybe StreamIndex     -- Find a stream related to this one
	-> m (Maybe StreamIndex)
findBestStream ctx mt mwanted mrelatedTo = do
	let wanted = maybe (-1) (\(StreamIndex i) -> i) mwanted
	let relatedTo = maybe (-1) (\(StreamIndex i) -> i) mrelatedTo
	r <- liftIO.withThis ctx$ \pctx ->
		av_find_best_stream pctx (fromCEnum mt) wanted relatedTo nullPtr 0
	if r < 0 then return Nothing else return.Just$ StreamIndex r

-- | Read a packet from a file.  Returns True on EOF, otherwise False.
readFrame :: (MonadIO m, MonadThrow m) =>
	AVFormatContext -> AVPacket -> m Bool
readFrame ctx pkt = do
	r <- liftIO$
		withThis ctx$ \pctx ->
		withThis pkt$ \ppkt -> av_read_frame pctx ppkt

	if r == 0 then return False
	else if toCEnum r == AVERROREof then return True
	else throwM$ mkError r "readFrame" "av_read_frame"

-- | Seek to a keyframe.  Returns True on success, otherwise False.
seekKeyframe :: MonadIO m =>
	AVFormatContext -> StreamIndex -> AVTimestamp -> AVSeekFlag -> m Bool
seekKeyframe ctx (StreamIndex idx) (AVTimestamp ts) flags = do
	r <- liftIO.withThis ctx$ \pctx -> av_seek_frame pctx idx ts (fromCEnum flags)
	return$ r >= 0

-- | Seek within a file.
seekFile :: MonadIO m =>
	AVFormatContext                              -- ^ Format context
	-> StreamIndex                               -- ^ Stream to seek
	-> (AVTimestamp, AVTimestamp, AVTimestamp)   -- ^ (min_ts, ts, max_ts)
	-> AVSeekFlag                                -- ^ flags
	-> m Bool                                    -- ^ True on success, otherwise False
seekFile ctx (StreamIndex idx) (AVTimestamp min_ts, AVTimestamp ts, AVTimestamp max_ts) flags = do
	r <- liftIO.withThis ctx$ \pctx ->
		avformat_seek_file pctx idx min_ts ts max_ts (fromCEnum flags)
	return$ r >= 0

-- | Discard all internally buffered data
-- formatFlush :: (MonadIO m, MonadThrow m) => AVFormatContext -> m ()
-- formatFlush ctx = do
-- 	r <- liftIO.withThis ctx$ avformat_flush
-- 	when (r < 0)$ throwM$
-- 		"formatFlush: avformat_flush failed with error code " ++ (show r)

-- | Start playing a network stream
readPlay :: (MonadIO m, MonadThrow m) => AVFormatContext -> m ()
readPlay ctx = do
	r <- liftIO.withThis ctx$ av_read_play
	when (r < 0)$ throwM$ mkError r "readPlay" "av_read_play"

-- | Pause a network stream
readPause :: (MonadIO m, MonadThrow m) => AVFormatContext -> m ()
readPause ctx = do
	r <- liftIO.withThis ctx$ av_read_pause
	when (r < 0)$ throwM$ mkError r "readPlay" "av_read_pause"

