{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
 
Description : Bindings to libavformat
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavformat.

-}

module Media.FFMpeg.Format.AVIO (
	Word24,

	ioFindProtocolName,
	ioCheck,

	AVIOWritable,
	ioWriteB,
	ioWriteL,

	AVIOReadable,
	ioReadL,
	ioReadB,

	ioSeek,
	ioSkip,
	ioSize,
	ioEOF,
	ioFlush,
	ioRead,
	ioWithURL,
	withppCtx,
	ioWithDynamicBuffer,
	ioEnumInputProtocols,
	ioEnumOutputProtocols,
	ioPause,
	ioResume,
	ioSeekTime
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Data.Bits
import Data.Int
import Data.Ix
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import System.IO.Unsafe

import Media.FFMpeg.Format.Core
import Media.FFMpeg.Format.Enums
import Media.FFMpeg.Internal.Common
import Media.FFMpeg.Util

--newtype AVIODirContext = AVIODirContext (Ptr AVIODirContext)
--data AVIODirEntry = AVIODirEntry {
--	dirEntry_name :: String,
--	dirEntry_type :: AVIODirEntryType,
--	dirEntry_size :: Int64,
--	dirEntry_modification_timestamp :: Int64,
--	dirEntry_access_timestamp :: Int64,
--	dirEntry_status_change_timestamp :: Int64,
--	dirEntry_user_id :: Int64,
--	dirEntry_group_id :: Int64,
--	dirEntry_filemode :: Int64
--}
newtype AVIOContext = AVIOContext (Ptr AVIOContext)
instance ExternalPointer AVIOContext where
	type UnderlyingType AVIOContext = AVIOContext
	withThis (AVIOContext ptr) = withThis ptr

foreign import ccall "avio_find_protocol_name" avio_find_protocol_name :: CString -> CString
foreign import ccall "avio_check" avio_check :: CString -> CInt -> IO CInt
-- foreign import ccall "avpriv_io_move" avpriv_io_move :: CString -> CString -> IO CInt
-- foreign import ccall "avpriv_io_delete" avpriv_io_delete :: CString -> IO CInt
-- foreign import ccall "avio_open_dir" avio_open_dir :: Ptr (Ptr AVIODirContext) -> CString -> Ptr (UnderlyingType AVDictionary) -> IO CInt
-- foreign import ccall "avio_read_dir" avio_read_dir :: Ptr AVIODirContext -> Ptr (Ptr AVIODirEntry) -> IO CInt
-- foreign import ccall "avio_close_dir" avio_close_dir :: Ptr (Ptr AVIODirContext) -> IO CInt
-- foreign import ccall "avio_free_directory_entry" avio_free_directory_entry :: Ptr (Ptr AVIODirEntry) -> IO ()
foreign import ccall "avio_alloc_context" avio_alloc_context ::
	Ptr CUChar -> CInt -> CInt -> Ptr () ->
	FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) ->
	FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO CInt) ->
	FunPtr (Ptr () -> Int64 -> CInt -> IO Int64) -> Ptr AVIOContext
foreign import ccall "avio_w8" avio_w8 :: Ptr AVIOContext -> CInt -> IO ()
foreign import ccall "avio_write" avio_write :: Ptr AVIOContext -> CString -> CInt -> IO ()
foreign import ccall "avio_wl64" avio_wl64 :: Ptr AVIOContext -> Word64 -> IO ()
foreign import ccall "avio_wb64" avio_wb64 :: Ptr AVIOContext -> Word64 -> IO ()
foreign import ccall "avio_wl32" avio_wl32 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb32" avio_wb32 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wl24" avio_wl24 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb24" avio_wb24 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wl16" avio_wl16 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_wb16" avio_wb16 :: Ptr AVIOContext -> CUInt -> IO ()
foreign import ccall "avio_put_str" avio_put_str :: Ptr AVIOContext -> CString -> IO CInt
foreign import ccall "avio_seek" avio_seek :: Ptr AVIOContext -> Int64 -> CInt -> IO Int64
foreign import ccall "avio_skip" avio_skip :: Ptr AVIOContext -> Int64 -> IO Int64
foreign import ccall "avio_size" avio_size :: Ptr AVIOContext -> IO Int64
foreign import ccall "avio_feof" avio_feof :: Ptr AVIOContext -> IO CInt
foreign import ccall "avio_flush" avio_flush :: Ptr AVIOContext -> IO ()
foreign import ccall "avio_read" avio_read :: Ptr AVIOContext -> CString -> CInt -> IO CInt
foreign import ccall "avio_get_str" avio_get_str :: Ptr AVIOContext -> CInt -> CString -> CInt -> IO CInt
foreign import ccall "avio_open" avio_open :: Ptr (Ptr AVIOContext) -> CString -> CInt -> IO CInt
foreign import ccall "avio_close" avio_close :: Ptr AVIOContext -> IO CInt
foreign import ccall "avio_closep" avio_closep :: Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_open_dyn_buf" avio_open_dyn_buf :: Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_close_dyn_buf" avio_close_dyn_buf :: Ptr AVIOContext -> Ptr (Ptr Word8) -> IO CInt
foreign import ccall "avio_enum_protocols" avio_enum_protocols :: Ptr (Ptr ()) -> CInt -> IO CString
foreign import ccall "avio_pause" avio_pause :: Ptr AVIOContext -> CInt -> IO CInt
foreign import ccall "avio_seek_time" avio_seek_time :: Ptr AVIOContext -> CInt -> Int64 -> CInt -> IO Int64
foreign import ccall "avio_accept" avio_accept :: Ptr AVIOContext -> Ptr (Ptr AVIOContext) -> IO CInt
foreign import ccall "avio_handshake" avio_handshake :: Ptr AVIOContext -> IO CInt

foreign import ccall "avio_r8" avio_r8 :: Ptr AVIOContext -> IO CInt
foreign import ccall "avio_rl16" avio_rl16 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rl24" avio_rl24 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rl32" avio_rl32 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rl64" avio_rl64 :: Ptr AVIOContext -> IO Word64
foreign import ccall "avio_rb16" avio_rb16 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rb24" avio_rb24 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rb32" avio_rb32 :: Ptr AVIOContext -> IO CUInt
foreign import ccall "avio_rb64" avio_rb64 :: Ptr AVIOContext -> IO Word64

type URL = String

-- | Get the protocol name from a URL
ioFindProtocolName :: URL -> String
ioFindProtocolName url = unsafePerformIO.withCString url$ peekCString.avio_find_protocol_name

-- | Check that the specified access is possible
ioCheck :: MonadIO m => URL -> AVIOOpenFlag -> m AVIOOpenFlag
ioCheck url mask = liftIO.withCString url$ \purl -> toCEnum <$> avio_check purl (fromCEnum mask)

-- | 24 bit word type
newtype Word24 = Word24 Word deriving (Eq, Ord, Read, Show, Num, Integral, Real, Ix, Bits)
instance Bounded Word24 where
	minBound = 0
	maxBound = (2^24) - 1
instance Enum Word24 where
	succ w@(Word24 x) = if w == maxBound then error "No successor" else Word24 (x + 1)
	pred w@(Word24 x) = if w == minBound then error "No predecessor" else Word24 (x - 1)
	enumFrom (Word24 x) = toEnum.fromIntegral <$> [x..((2^24)-1)]
	enumFromThen (Word24 a) (Word24 b) = toEnum.fromIntegral <$> [a,b..((2^24)-1)]
	toEnum = Word24 . fromIntegral
	fromEnum (Word24 x) = fromIntegral x
instance Storable Word24 where
	sizeOf _ = 3
	alignment _ = 4
	peek ptr = do
		v <- peek (castPtr ptr)
		return.Word24$ v .&. 0xFFFFFF
	poke ptr (Word24 v) = do
		x <- peek (castPtr ptr)
		poke (castPtr ptr) ((x .&. mask) .|. (v .&. 0xFFFFFF))
		where mask = (maxBound `unsafeShiftR` 24) `unsafeShiftL` 24

-- | Types that can be written directly to an AVIO stream
class AVIOWritable t where
	avio_store_b :: Ptr AVIOContext -> t -> IO ()
	avio_store_l :: Ptr AVIOContext -> t -> IO ()

instance AVIOWritable B.ByteString where
	avio_store_b ctx v = B.useAsCStringLen v$ \(pv, len) -> avio_write ctx pv (fromIntegral len)
	avio_store_l = avio_store_b

instance AVIOWritable Word64 where
	avio_store_b ctx v = avio_wb64 ctx v
	avio_store_l ctx v = avio_wl64 ctx v

instance AVIOWritable Word32 where
	avio_store_b ctx v = avio_wb32 ctx (fromIntegral v)
	avio_store_l ctx v = avio_wl32 ctx (fromIntegral v)

instance AVIOWritable Word24 where
	avio_store_b ctx v = avio_wb24 ctx (fromIntegral v)
	avio_store_l ctx v = avio_wl24 ctx (fromIntegral v)

instance AVIOWritable Word16 where
	avio_store_b ctx v = avio_wb16 ctx (fromIntegral v)
	avio_store_l ctx v = avio_wl16 ctx (fromIntegral v)

instance AVIOWritable Word8 where
	avio_store_b ctx v = avio_w8 ctx (fromIntegral v)
	avio_store_l = avio_store_b

-- | Write big endian
ioWriteB :: (MonadIO m, AVIOWritable t) => AVIOContext -> t -> m ()
ioWriteB ctx v = withThis ctx$ \pctx -> liftIO$ avio_store_b pctx v

-- | Write little endian
ioWriteL :: (MonadIO m, AVIOWritable t) => AVIOContext -> t -> m ()
ioWriteL ctx v = withThis ctx$ \pctx -> liftIO$ avio_store_l pctx v

-- | Seek an AVIO stream
ioSeek :: (MonadIO m, MonadError String m) => AVIOContext -> Int64 -> AVIOSeek -> m Int64
ioSeek ctx i whence = withThis ctx$ \pctx -> do
	r <- liftIO$ avio_seek pctx i (fromCEnum whence)
	when (r < 0)$ throwError$ "ioSeek: avio_seek failed with error code " ++ (show r)
	return r

-- | Skip bytes in an AVIO stream
ioSkip :: (MonadIO m, MonadError String m) => AVIOContext -> Int64 -> m Int64
ioSkip ctx i = withThis ctx$ \pctx -> do
	r <- liftIO$ avio_skip pctx i
	when (r < 0)$ throwError$ "ioSkip: avio_skip failed with error code " ++ (show r)
	return r

-- | Get the filesize of an AVIO stream
ioSize :: (MonadIO m, MonadError String m) => AVIOContext -> m Int64
ioSize ctx = withThis ctx$ \pctx -> do
	r <- liftIO$ avio_size pctx
	when (r < 0)$ throwError$ "ioSize: avio_size failed with error code " ++ (show r)
	return r

-- | Determine if we are at the end of the file
ioEOF :: MonadIO m => AVIOContext -> m Bool
ioEOF ctx = liftIO.withThis ctx$ \pctx -> fmap (/= 0)$ avio_feof pctx

-- | Flush all buffered data
ioFlush :: MonadIO m => AVIOContext -> m ()
ioFlush ctx = liftIO.withThis ctx$ \pctx -> avio_flush pctx

-- | Read from an AVIO stream
ioRead :: (MonadIO m, MonadError String m) => AVIOContext -> Int -> m B.ByteString
ioRead ctx len = withThis ctx$ \pctx -> do
	(r, s) <- liftIO.allocaBytes len$ \pbuffer -> do
		r <- avio_read pctx pbuffer (fromIntegral len)
		s <- B.packCStringLen (pbuffer, if r < 0 then 0 else fromIntegral r)
		return (r, s)
	when (r < 0)$ throwError$
		"ioRead: avio_read failed with error code " ++ (show r)
	return s

-- | Types that can be read directly from an AVIO stream
class AVIOReadable t where
	avio_read_b :: Ptr AVIOContext -> IO t
	avio_read_l :: Ptr AVIOContext -> IO t

instance AVIOReadable Word8 where
	avio_read_b = fmap fromIntegral. avio_r8
	avio_read_l = avio_read_b
	
instance AVIOReadable Word16 where
	avio_read_b = fmap fromIntegral.avio_rl16
	avio_read_l = fmap fromIntegral.avio_rb16

instance AVIOReadable Word24 where
	avio_read_b = fmap fromIntegral.avio_rl24
	avio_read_l = fmap fromIntegral.avio_rb24

instance AVIOReadable Word32 where
	avio_read_b = fmap fromIntegral.avio_rl32
	avio_read_l = fmap fromIntegral.avio_rb32

instance AVIOReadable Word64 where
	avio_read_b = avio_rl64
	avio_read_l = avio_rb64

-- | Read a big endian value from an AVIOContext
ioReadL :: (MonadIO m, AVIOReadable t) => AVIOContext -> m t
ioReadL ctx = liftIO$ withThis ctx avio_read_l

-- | Read a little endian value from an AVIOContext
ioReadB :: (MonadIO m, AVIOReadable t) => AVIOContext -> m t
ioReadB ctx = liftIO$ withThis ctx avio_read_b

-- | Execute an action with a newly opened AVIOContext
ioWithURL :: (MonadIO m, MonadError String m) =>
	Maybe AVFormatContext -> URL -> AVIOOpenFlag -> (AVIOContext -> m b) -> m b
ioWithURL mf url flags action = 
	withppCtx mf$ \ppctx ->
	withThis url$ \purl -> do
		r1 <- liftIO$ avio_open ppctx purl (fromCEnum flags)
		when (r1 < 0)$ throwError$ "ioWithURL: avio_open failed with error code " ++ (show r1)
		ret <- action =<< (liftIO$ AVIOContext <$> peek ppctx)
		r2 <- liftIO$ avio_closep ppctx
		when (r2 < 0)$ throwError$ "ioWithURL: avio_closep failed with error code " ++ (show r2)
		return ret

-- | Perform an action with a pointer to a pointer to an AVIOContext
withppCtx :: (MonadIO m, MonadError String m) => Maybe AVFormatContext -> (Ptr (Ptr AVIOContext) -> m b) -> m b
withppCtx mf = case mf of
	Nothing -> \action -> do
		p <- liftIO$ malloc
		r <- action p `catchError` (\e -> do
			liftIO$ free p
			throwError e)
		liftIO$ free p
		return r
	Just ctx -> \action -> withThis ctx (action.(`plusPtr` #{offset AVFormatContext, pb}))

-- | Execute an action with an in memory AVIOContext
ioWithDynamicBuffer :: (MonadIO m, MonadError String m) =>
	Maybe AVFormatContext -> (AVIOContext -> m b) -> m (b, B.ByteString)
ioWithDynamicBuffer mf action =
	withppCtx mf$ \ppctx -> do
		r <- liftIO$ avio_open_dyn_buf ppctx
		when (r /= 0)$ throwError$
			"ioWithDynamicBuffer: avio_open_dyn_buf failed with error code " ++ (show r)

		pctx <- liftIO$ peek ppctx
		ret <- action$ AVIOContext pctx

		d <- liftIO.alloca$ \ppbuff -> do
			s <- avio_close_dyn_buf pctx ppbuff
			pbuff <- peek ppbuff
			ret <- B.packCStringLen (castPtr pbuff, fromIntegral s)
			av_free pbuff
			return ret

		return (ret, d)

-- | Enumerate the output protocols
ioEnumInputProtocols :: MonadIO m => m [String]
ioEnumInputProtocols = ioEnumProtocols False

-- | Enumerate the input protocols
ioEnumOutputProtocols :: MonadIO m => m [String]
ioEnumOutputProtocols = ioEnumProtocols True

-- | Enumerate the protocols
ioEnumProtocols :: MonadIO m => Bool -> m [String]
ioEnumProtocols io = liftIO.alloca$ \opaque -> do
	let allProtocols = do
		p <- avio_enum_protocols opaque flag
		if p == nullPtr then return [] else (p :) <$> allProtocols

	mapM peekCString =<< allProtocols

	where flag = if io then 1 else 0

-- | Pause a network stream
ioPause :: MonadIO m => AVIOContext -> m ()
ioPause ctx = do
	liftIO.withThis ctx$ \pctx -> avio_pause pctx 1
	return ()

-- | Resume a network stream
ioResume :: MonadIO m => AVIOContext -> m ()
ioResume ctx = do
	liftIO.withThis ctx$ \pctx -> avio_pause pctx 0
	return ()

-- | Seek a network stream
ioSeekTime :: (MonadIO m, MonadError String m) =>
	AVIOContext -> Maybe StreamIndex -> AVTimestamp -> AVSeekFlag -> m ()
ioSeekTime ctx msi (AVTimestamp ts) flags =
	withThis ctx$ \pctx -> do
		r <- liftIO$ avio_seek_time pctx si ts (fromCEnum flags)
		when (r < 0)$ throwError$ "ioSeekTime: avio_seek_time failed with error code " ++ (show r)

	where si = case msi of
		Nothing -> -1
		Just (StreamIndex i) -> i

