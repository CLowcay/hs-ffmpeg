-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

{- |Module 'Media.FFMpeg.Common' common definitions and helpers

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.Common (
	ExternalPointer (..),
	CEnum (..),
	fromVersionNum,
	cToInt,
	cFromInt,
	justPtr
) where

import Data.Bits
import Data.Version
import Foreign.C.Types
import Foreign.Ptr

-- 
-- |ExternalPointer class, used to define method withThis
-- similar to 'Foreign.Marshal.Utils.with' but without need Storable specifier
--

class ExternalPointer a where
    withThis :: a -> (Ptr b -> IO c) -> IO c

-- For internal use only
class CEnum a where
	fromCEnum :: a -> CInt
	toCEnum :: CInt -> a

fromVersionNum :: Int -> Version
fromVersionNum v = Version {
  versionBranch = map (flip (.&.) 0xFF . shift v) [(-16), (-8), 0],
  versionTags = [] }

--
-- Some conversion functions
--
cToInt :: (Integral a) => a -> Int
cToInt = fromIntegral

cFromInt :: (Num a) => Int -> a 
cFromInt = fromIntegral

justPtr :: Ptr a -> Maybe (Ptr a)
justPtr p | p == nullPtr  = Nothing
          | otherwise = Just p

