{-# LANGUAGE ForeignFunctionInterface #-}

{- |
 
Description : Bindings to libavutil
Copyright   : (c) Callum Lowcay, 2015
License     : BSD3
Stability   : experimental

Bindings to libavutil

-}

module Media.FFMpeg.Util.Buffer (
	av_buffer_alloc,
	av_buffer_allocz,
	av_buffer_create,
	av_buffer_default_free,
	av_buffer_ref,
	av_buffer_unref,
	av_buffer_is_writable,
	av_buffer_get_opaque,
	av_buffer_get_ref_count,
	av_buffer_make_writable,
	av_buffer_realloc,

	AVBufferRef,

	getBufferData,
	withBufferData,
	av_buffer_flag_readonly,

	mkBufferRef,
	refBufferRef,
	getBufferRef,
	allocBufferRef,
	alloczBufferRef,
	bufferIsWritable,
	bufferMakeWritable
) where

#include "ffmpeg.h"

import Control.Applicative
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Media.FFMpeg.Util.Error

-- | AVBufferRef struct
newtype AVBufferRef = AVBufferRef (ForeignPtr (Ptr AVBufferRef))

-- | Get a pointer to the actual buffer data
getBufferData :: MonadIO m => Ptr AVBufferRef -> m (Ptr Word8, Int)
getBufferData pbuff = liftIO$ do
	d <- #{peek AVBufferRef, data} pbuff
	s <- fromIntegral <$> (#{peek AVBufferRef, size} pbuff :: IO CInt)
	return (d, s)

-- | Safely run an action on buffer data
withBufferData :: MonadIO m => AVBufferRef -> ((Ptr Word8, Int) -> m b) -> m b
withBufferData (AVBufferRef fp) action = do
	let ppbuff = unsafeForeignPtrToPtr fp
	r <- action =<< getBufferData =<< liftIO (peek ppbuff)
	liftIO$ touchForeignPtr fp
	return r

-- | AV_BUFFER_FLAG_READONLY
av_buffer_flag_readonly :: Int
av_buffer_flag_readonly = #{const AV_BUFFER_FLAG_READONLY}

foreign import ccall "av_buffer_alloc" av_buffer_alloc :: CInt -> IO (Ptr AVBufferRef)
foreign import ccall "av_buffer_allocz" av_buffer_allocz :: CInt -> IO (Ptr AVBufferRef)
foreign import ccall "av_buffer_create" av_buffer_create :: Ptr Word8 -> CInt -> FunPtr (Ptr () -> Ptr Word8 -> IO ())
	-> Ptr () -> CInt -> IO (Ptr AVBufferRef)
foreign import ccall "av_buffer_default_free" av_buffer_default_free :: Ptr () -> Ptr Word8 -> IO ()
foreign import ccall "av_buffer_ref" av_buffer_ref :: Ptr AVBufferRef -> IO (Ptr AVBufferRef)
foreign import ccall "av_buffer_unref" av_buffer_unref :: Ptr (Ptr AVBufferRef) -> IO ()
foreign import ccall "&av_buffer_unref" pav_buffer_unref :: FunPtr (Ptr (Ptr AVBufferRef) -> IO ())
foreign import ccall "av_buffer_is_writable" av_buffer_is_writable :: Ptr AVBufferRef -> IO CInt
foreign import ccall "av_buffer_get_opaque" av_buffer_get_opaque :: Ptr AVBufferRef -> IO (Ptr ())
foreign import ccall "av_buffer_get_ref_count" av_buffer_get_ref_count :: Ptr AVBufferRef -> IO CInt
foreign import ccall "av_buffer_make_writable" av_buffer_make_writable :: Ptr (Ptr AVBufferRef) -> IO CInt
foreign import ccall "av_buffer_realloc" av_buffer_realloc :: Ptr (Ptr AVBufferRef) -> CInt -> IO CInt

-- | Make a new reference to a buffer from a pointer
mkBufferRef :: (MonadIO m, MonadThrow m) =>
	Ptr AVBufferRef -> m AVBufferRef
mkBufferRef pbuff = do
	r <- liftIO$ av_buffer_ref pbuff
	when (r == nullPtr)$ throwM$
		mkNullPointerError "mkBufferRef" "av_buffer_ref"

	fp <- liftIO$ mallocForeignPtr
	liftIO.withForeignPtr fp$ \ptr -> poke ptr r
	liftIO$ addForeignPtrFinalizer pav_buffer_unref fp

	return$ AVBufferRef fp

-- | Make a new reference to a buffer
refBufferRef :: (MonadIO m, MonadThrow m) =>
	AVBufferRef -> m AVBufferRef
refBufferRef (AVBufferRef fp) = do
	r <- liftIO.withForeignPtr fp$ av_buffer_ref <=< peek
	when (r == nullPtr)$ throwM$
		mkNullPointerError "refBufferRef" "av_buffer_ref"

	fp <- liftIO$ mallocForeignPtr
	liftIO.withForeignPtr fp$ \ptr -> poke ptr r
	liftIO$ addForeignPtrFinalizer pav_buffer_unref fp

	return$ AVBufferRef fp

-- | Get a pointer to a buffer reference.  Does not create a new reference to
-- the buffer
getBufferRef :: MonadIO m => AVBufferRef -> (Ptr AVBufferRef -> m b) -> m b
getBufferRef (AVBufferRef fp) action = do
	let ppbuf = unsafeForeignPtrToPtr fp
	r <- action =<< liftIO (peek ppbuf)
	liftIO$ touchForeignPtr fp
	return r

-- | Allocate a new buffer
allocBufferRef :: (MonadIO m, MonadThrow m) =>
	Int -> (Ptr AVBufferRef -> m b) -> m b
allocBufferRef size action = do
	pbuff <- liftIO$ av_buffer_alloc (fromIntegral size)
	when (pbuff == nullPtr)$ throwM$
		mkNullPointerError "allocBufferRef" "av_buffer_alloc"
	action pbuff

-- | Allocate and zero a new buffer
alloczBufferRef :: (MonadIO m, MonadThrow m) =>
	Int -> (Ptr AVBufferRef -> m b) -> m b
alloczBufferRef size action = do
	pbuff <- liftIO$ av_buffer_allocz (fromIntegral size)
	when (pbuff == nullPtr)$ throwM$
		mkNullPointerError "alloczBufferRef" "av_buffer_allocz"
	action pbuff

-- | Determine if a buffer is writable
bufferIsWritable :: MonadIO m => Ptr AVBufferRef -> m Bool
bufferIsWritable pbuff = liftIO$ (== 1) <$> av_buffer_is_writable pbuff

-- | Make a buffer writable, copying all the data if necessary
bufferMakeWritable :: (MonadIO m, MonadThrow m) =>
	Ptr AVBufferRef -> m (Ptr AVBufferRef)
bufferMakeWritable pbuff = do
	(pbuff', r) <- liftIO.alloca$ \ppbuff -> do
		r <- av_buffer_make_writable ppbuff
		pbuff' <- peek ppbuff
		return (pbuff', r)
	
	when (r /= 0)$ throwM$
		mkError r "bufferMakeWritable" "av_buffer_make_writable"
	
	return pbuff'

