-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

{- |
	Module 'Media.FFMpeg.Util' implements bindings to AVUtil library

	(c) 2009 Vasyl Pasternak
-}

module Media.FFMpeg.Util (
	module Media.FFMpeg.Util.Enums,
	avMalloc,
	avFree,
	newAvForeignPtr,
	Buffer,
	withBuffer,
	bufferSize,
	allocBuffer,
	shiftBuffer,
	--castBuffer,
	castBufferSize  -- used internally
	--unsafeBufferSetSize
) where

import Control.Applicative
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Error
import Foreign.Ptr
import Text.Printf

import Media.FFMpeg.Common
import Media.FFMpeg.Util.Enums

#include "ffmpeg.h"

-- | av_free
foreign import ccall "av_free" avFree :: Ptr a -> IO ()

-- | avMalloc 
avMalloc :: Integral a => a -> IO (Maybe (Ptr ()))
avMalloc a = do 
	ptr <- _malloc (fromIntegral a)
	return $ if ptr == nullPtr then Nothing else Just ptr 

foreign import ccall "av_malloc" _malloc :: CUInt -> IO (Ptr ())

-- | create new foreign ptr using av_free for finalization
newAvForeignPtr :: Ptr a -> IO (ForeignPtr a)
newAvForeignPtr p = newFinForeignPtr avFree p

-- | Buffer type
data Buffer = Buffer Int (ForeignPtr Buffer) 

instance ExternalPointer Buffer where
	withThis (Buffer _ b) io = withForeignPtr b (io . castPtr)

-- | operations with buffer
withBuffer :: Buffer -> (Ptr a -> IO b) -> IO b
withBuffer = withThis

-- | Get the size of a buffer
bufferSize :: Buffer -> Int
bufferSize (Buffer s _) = s

-- | Advance the buffer pointer.  The memory backing this Buffer will be freed
-- after the original Buffer object has been garbage collected, which could be
-- problematic.  This function should be revisited
shiftBuffer :: Int -> Buffer -> IO Buffer
shiftBuffer l buf
		| l > bsize = error $
			printf "shiftBuffer: buffer is too narrow, size is %d, shift %d\n"
				bsize l
		| otherwise = withThis buf $ castBufferSize (bsize - l) . (`plusPtr` l)
	where bsize = bufferSize buf

-- | Allocate buffer with specified size
allocBuffer :: Int -> IO Buffer
allocBuffer size = do
	p <- throwIf 
		(== nullPtr)
		(\_ -> "allocBuffer: failed to allocate memory block")
		(_malloc (fromIntegral size))
	(Buffer size . castForeignPtr) <$> newAvForeignPtr p

-- | Cast pointer to buffer with size (unsafe op)
castBufferSize :: Int -> Ptr a -> IO Buffer
castBufferSize s = fmap (Buffer s . castForeignPtr) . newForeignPtr_

-- | Cast pointer to buffer (unsafe operation)
castBuffer :: Ptr a -> IO Buffer
castBuffer = castBufferSize (-1)

-- | Very unsafe operation, allows to set size to the 
-- existing buffer. Use very carefully
unsafeBufferSetSize :: Buffer -> Int -> Buffer
unsafeBufferSetSize (Buffer s b) ns = Buffer ns b

