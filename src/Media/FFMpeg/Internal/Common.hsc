-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface #-}

{- |

Description : Internal utility module for the ffmpeg bindings
Copyright   : (c) Vasyl Pasternak, 2009
License     : BSD3
Stability   : experimental

Internal utility module for the ffmpeg bindings

-}

module Media.FFMpeg.Internal.Common (
	ExternalPointer (..),
	CEnum (..),
	fromVersionNum,
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

-- |Returns Nothing in the case the p == nullPtr
justPtr :: Ptr a -> Maybe (Ptr a)
justPtr p | p == nullPtr  = Nothing
          | otherwise = Just p

