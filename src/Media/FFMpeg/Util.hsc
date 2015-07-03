-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

{- |Module 'Media.FFMpeg.Util' implements bindings to AVUtil library

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.Util 
    (
     module Media.FFMpeg.UtilEnums_

    ,avMalloc
    ,avFree

    ,mkFinalizerPtr
    ,avFinalizer
    ,newAvForeignPtr
    ,newFinForeignPtr

    ,Buffer
    ,bufferSize
    ,allocBuffer
    ,shiftBuffer
    ,castBuffer
    ,castBufferSize
    ,withBuffer
    ,unsafeBufferSetSize
    )where

import Foreign
import Foreign.C.Types
import Control.Monad (liftM)
import Text.Printf

import Media.FFMpeg.Common
import Media.FFMpeg.UtilEnums_

#include "ffmpeg.h"


--
-- |avFree
-- 
foreign import ccall "av_free" avFree :: Ptr a -> IO ()


--
-- |avMalloc 
-- 
foreign import ccall "av_malloc" _malloc :: CUInt -> IO (Ptr())

avMalloc :: Integral a => a -> IO (Maybe (Ptr()))
avMalloc a = do 
  ptr <- _malloc (fromIntegral a)
  return $ if ptr == nullPtr then Nothing else Just ptr 

--
-- |mkFinalizerPtr
--
foreign import ccall "wrapper" mkFinalizerPtr :: 
    (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

--
-- |avFinalizer -- finalizer function pointer to avFree
--
avFinalizer :: IO (FunPtr (Ptr a -> IO ()))
avFinalizer = mkFinalizerPtr avFree

--
-- |newAvForeignPtr create new foreign ptr with avFinalizer
-- 
newAvForeignPtr :: Ptr a -> IO (ForeignPtr a)
newAvForeignPtr p = avFinalizer >>= \f -> newForeignPtr f p

newFinForeignPtr :: (Ptr a -> IO ()) -> Ptr a -> IO (ForeignPtr a)
newFinForeignPtr f p = mkFinalizerPtr f >>= \f' -> newForeignPtr f' p

--
-- |Buffer type
--
data Buffer = Buffer Int (ForeignPtr Buffer) 

instance ExternalPointer Buffer where
    withThis (Buffer _ b) io = withForeignPtr b (io . castPtr)

bufferSize :: Buffer -> Int
bufferSize (Buffer s _) = s

shiftBuffer :: Int -> Buffer -> IO Buffer
shiftBuffer l buf 
    | l > bsize = error $
                    printf "shiftBuffer: buffer is to narrow, size is %d, shift %d\n" 
                           bsize l
    | otherwise = withThis buf $ castBufferSize (bsize - l) . (`plusPtr` l) 
    where bsize = bufferSize buf
        

--
-- Allocate buffer with specified size
--
allocBuffer :: Int -> IO Buffer
allocBuffer size = do
  p <- throwIf 
       (== nullPtr)
       (\_ -> "allocBuffer: failed to allocate memory block")
       (_malloc (fromIntegral size))
  liftM (Buffer size . castForeignPtr) $ newAvForeignPtr p

-- |Cast pointer to buffer with size (unsafe op)
castBufferSize :: Int -> Ptr a -> IO Buffer
castBufferSize s = liftM (Buffer s . castForeignPtr) . newForeignPtr_

-- |Cast pointer to buffer (unsafe operation)
castBuffer :: Ptr a -> IO Buffer
castBuffer = castBufferSize (-1)

-- |operations with buffer
withBuffer :: ExternalPointer p => p -> (Ptr a -> IO b) -> IO b
withBuffer = withThis

-- | Very unsafe operation, allows to set size to the 
-- existing buffer. Use very carefully
unsafeBufferSetSize :: Buffer -> Int -> Buffer
unsafeBufferSetSize (Buffer s b) ns = Buffer ns b

