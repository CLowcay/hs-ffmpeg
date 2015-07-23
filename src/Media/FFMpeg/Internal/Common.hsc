{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}

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
	CFlags (..),
	fromVersionNum,
	justPtr
) where

import Data.Bits
import Data.Monoid
import Data.Version
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

-- | Similar to 'Foreign.Marshal.Utils.with' but without needing the Storable
-- constraint
class ExternalPointer a where
	withThis :: a -> (Ptr b -> IO c) -> IO c

-- | Used for marshalling enumerations and flags
class CEnum a where
	fromCEnum :: a -> CInt
	toCEnum :: CInt -> a

instance CEnum CInt where
	fromCEnum = id
	toCEnum = id

-- | Class for working with flags
class CFlags a where
	funion :: a -> a -> a
	fintersection :: a -> a -> a
	fminus :: a -> a -> a
	fhasAny :: a -> a -> Bool
	fhasAll :: a -> a -> Bool
	fhas :: a -> a -> Bool
	fhas = fhasAll
	flagsToInt :: a -> CInt
	fempty :: a

instance CFlags CInt where
	funion = (.|.)
	fintersection = (.&.)
	x `fminus` y = x .&. (complement y)
	x `fhasAny` y = (x .&. y) /= 0
	x `fhasAll` y = (x .&. y) == y
	flagsToInt = fromIntegral
	fempty = 0

instance CFlags Word64 where
	funion = (.|.)
	fintersection = (.&.)
	x `fminus` y = x .&. (complement y)
	x `fhasAny` y = (x .&. y) /= 0
	x `fhasAll` y = (x .&. y) == y
	flagsToInt = fromIntegral
	fempty = 0

instance CFlags Word32 where
	funion = (.|.)
	fintersection = (.&.)
	x `fminus` y = x .&. (complement y)
	x `fhasAny` y = (x .&. y) /= 0
	x `fhasAll` y = (x .&. y) == y
	flagsToInt = fromIntegral
	fempty = 0

instance CFlags a => Monoid a where
	mempty = fempty
	mappend = funion

-- | Parse libAV version numbers
fromVersionNum :: Int -> Version
fromVersionNum v =
	Version {
		versionBranch = map (flip (.&.) 0xFF . shift v) [(-16), (-8), 0],
		versionTags = []
	}

-- |Returns Nothing in the case the p == nullPtr
justPtr :: Ptr a -> Maybe (Ptr a)
justPtr p | p == nullPtr  = Nothing
          | otherwise = Just p

