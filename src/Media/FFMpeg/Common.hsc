-- -*- haskell -*-
{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}

{- |Module 'Media.FFMpeg.Common' common definitions and helpers

   (c) 2009 Vasyl Pasternak
 -}

module Media.FFMpeg.Common 
    (
     ExternalPointer (..)
    ,fromVersionNum
    ,cToInt
    ,cFromInt
    ,cToEnum
    ,cFromEnum
    ,justPtr
    ,combineBitMasks
    )where

-- 
-- |ExternalPointer class, used to define method withThis
-- similar to 'Foreign.Marshal.Utils.with' but without need Storable specifier
--

import Foreign.Ptr (Ptr, nullPtr)
import Data.Bits (Bits, (.|.), (.&.), shift)
import Data.Version (Version (..))


class ExternalPointer a where
    withThis :: a -> (Ptr b -> IO c) -> IO c

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

cToEnum :: (Integral a, Enum b) => a -> b
cToEnum = toEnum . fromIntegral

cFromEnum :: (Enum a, Num b) => a -> b
cFromEnum = fromIntegral . fromEnum

justPtr :: Ptr a -> Maybe (Ptr a)
justPtr p | p == nullPtr  = Nothing
          | otherwise = Just p

combineBitMasks :: (Enum a, Bits b) => [a] -> b
combineBitMasks = foldl (.|.) 0 . map cFromEnum
