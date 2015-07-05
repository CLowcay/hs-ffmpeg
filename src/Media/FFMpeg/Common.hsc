-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
	Module 'Media.FFMpeg.Common' common definitions and helpers

	(c) 2009 Vasyl Pasternak
-}

module Media.FFMpeg.Common (
	ExternalPointer (..),
	mkFinalizerPtr,
	newFinForeignPtr,
	CEnum (..),
	fromVersionNum,
	cToInt,
	cFromInt,
	justPtr
) where

import Data.Bits
import Data.Version
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

-- |Used to define method withThis
-- similar to 'Foreign.Marshal.Utils.with' but without need Storable specifier
class ExternalPointer a where
    withThis :: a -> (Ptr b -> IO c) -> IO c

-- | create a new foreign ptr using a particular finalizer
newFinForeignPtr :: (Ptr a -> IO ()) -> Ptr a -> IO (ForeignPtr a)
newFinForeignPtr f p = mkFinalizerPtr f >>= \f' -> newForeignPtr f' p

foreign import ccall "wrapper" mkFinalizerPtr :: 
	(Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

-- |Used for marshalling enumerations and flags
-- For internal use only
class CEnum a where
	fromCEnum :: a -> CInt
	toCEnum :: CInt -> a

-- |Parse libAV version numbers
fromVersionNum :: Int -> Version
fromVersionNum v = Version {
  versionBranch = map (flip (.&.) 0xFF . shift v) [(-16), (-8), 0],
  versionTags = [] }

-- |Convert C types to Int
cToInt :: (Integral a) => a -> Int
cToInt = fromIntegral

-- |Convert Int to a C type
cFromInt :: (Num a) => Int -> a 
cFromInt = fromIntegral

-- |Returns Nothing in the case the p == nullPtr
justPtr :: Ptr a -> Maybe (Ptr a)
justPtr p | p == nullPtr  = Nothing
          | otherwise = Just p

